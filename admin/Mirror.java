import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.AccessDeniedException;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.HexFormat;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.UnaryOperator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Bidirektionale Spiegelung zweier lokaler Verzeichnisbaeume, gedacht fuer einen Eclipse-Workspace
 * und seine Kopie in einer zweiten Eclipse-Instanz.
 *
 * <pre>
 *   java Mirror.java sync &lt;A&gt; &lt;B&gt; [--dry-run] [--verbose]
 * </pre>
 */
public class Mirror {

    static final String USAGE = """
            Aufruf: java Mirror.java sync <A> <B> [--map <praefixA>=<praefixB>] [--replace <textA>=<textB>]
                                       [--remap] [--dry-run] [--verbose]
              --map       Paket-Praefix in A und B, z. B. --map net.abit=de.kunde.abit (Pfade + .java-Inhalt)
                          (ausgenommen: Basis/Garnishment/docs wird immer 1:1 gespiegelt)
              --replace   Text in A und B, in allen Textdateien ersetzt, z. B. --replace net/sf/jailer=net/abit/gtool
                          (woertlich, Inhalt der Textdateien und Dateinamen, nicht Ordnernamen;
                           hoechstens zweimal, A->B in Reihenfolge, B->A umgekehrt; ausgenommen wie bei --map)
              --remap     Abbildung bewusst aendern (sonst Abbruch, wenn sie vom Abgleichstand abweicht)
              --dry-run   nur anzeigen, was passieren wuerde (keine Datei- und keine State-Aenderung)
              --verbose   jede Aktion einzeln ausgeben
            Ausschluesse: feste Defaults plus Glob-Zeilen aus <A>/.mirrorignore und <B>/.mirrorignore
            Eclipse-Dateien (.project, .classpath, .factorypath, .settings/) nur bei der Erstkopie""";

    static final Set<String> DEFAULT_EXCLUDED_DIRS = Set.of(
            ".git", ".gradle", "build", "bin", "classes", "node_modules", ".codegraph", ".mirror", "api");
    static final String TMP_SUFFIX = ".mirror-tmp";
    static final String IGNORE_FILE = ".mirrorignore";
    static final String STATE_HEADER_V1 = "# eclipse-mirror v1";
    static final String STATE_HEADER_V2 = "# eclipse-mirror v2";
    static final String STATE_HEADER = "# eclipse-mirror v3";
    static final int MAX_PRINTED_ERRORS = 200;
    static final String GIT_EXCLUDE_LINE = "/.mirror/";
    static final Set<String> ECLIPSE_FILES = Set.of(".project", ".classpath", ".factorypath");
    static final String ECLIPSE_SETTINGS_DIR = ".settings";
    /** Verzeichnisse, die bei --map unveraendert gespiegelt werden (weder Pfad noch Inhalt umgeschrieben). */
    static final List<String> NO_MAP_DIRS = List.of("basis/garnishment/docs");
    static final Pattern EXCLUDE_FILES = Pattern.compile(
            "^(build\\.xml"
            + "|jailer(GUI)?\\.(bat|sh)"
            + "|config/log4j2?\\.xml"
            + "|admin/[^/]+\\.(java|sh|xml)"
            + "|\\.dockerignore|\\.gitignore|\\.classpath|\\.project)$");
    /** Endungen der Textdateien, in denen --replace ersetzt. */
    static final Set<String> TEXT_EXTENSIONS = Set.of(
            "java", "json", "xml", "form", "properties", "txt", "md", "html", "htm", "css", "js", "sh", "bat", "cmd",
            "sql", "csv", "tsv", "yml", "yaml", "gradle", "jm", "mf", "ini", "conf", "cfg", "inc", "jsp", "pom");

    enum Side {
        A, B;

        Side other() {
            return this == A ? B : A;
        }
    }

    enum Status { UNCHANGED, TOUCHED, CHANGED, DELETED }

    enum Kind { COPY, DELETE, RECORD, FORGET }

    record FileInfo(long size, long mtime) {
    }

    /** Abgleichstand einer Datei: gemeinsamer Inhalt (hash) und je Seite Groesse/mtime nach dem letzten Abgleich. */
    record Entry(String hash, long sizeA, long mtimeA, long sizeB, long mtimeB) {
        long size(Side s) {
            return s == Side.A ? sizeA : sizeB;
        }

        long mtime(Side s) {
            return s == Side.A ? mtimeA : mtimeB;
        }

        static Entry of(String hash, FileInfo a, FileInfo b) {
            return new Entry(hash, a.size(), a.mtime(), b.size(), b.mtime());
        }
    }

    /** COPY: side = Quelle; DELETE: side = Seite, auf der geloescht wird; RECORD: hash = gemeinsamer Inhalt. */
    record Action(Kind kind, String rel, Side side, boolean backup, String hash, String note) {
    }

    record Observed(Status status, String hash) {
    }

    record IgnoreRule(Pattern pattern, boolean dirOnly, boolean matchName) {
    }

    /** Abbildung des Paket-Praefix von A nach B; {@link #IDENTITY}, wenn beide gleich heissen. */
    static final class PrefixMap {
        static final PrefixMap IDENTITY = new PrefixMap("", "");
        static final Pattern QUALIFIED_NAME = Pattern.compile("[A-Za-z_$][\\w$]*(\\.[A-Za-z_$][\\w$]*)*");

        final String a;
        final String b;
        private final Pattern pathA;
        private final Pattern pathB;
        private final Pattern[] javaA;
        private final Pattern[] javaB;
        /** Alle Formen des B-Praefix in einem Pattern, fuer {@link #outsideB}. */
        private final Pattern anyB;

        PrefixMap(String a, String b) {
            this.a = a;
            this.b = b;
            this.pathA = pathPattern(a);
            this.pathB = pathPattern(b);
            this.javaA = javaPatterns(a);
            this.javaB = javaPatterns(b);
            this.anyB = javaB == null ? null
                    : Pattern.compile(javaB[0].pattern() + "|" + javaB[1].pattern() + "|" + javaB[2].pattern());
        }

        /**
         * Wendet op nur auf die Abschnitte zwischen den Vorkommen des B-Praefix an; die Vorkommen selbst bleiben
         * unveraendert. So schreibt --replace nicht in die Praefixe, die --map erzeugt hat.
         */
        String outsideB(String content, UnaryOperator<String> op) {
            if (anyB == null) {
                return op.apply(content);
            }
            Matcher m = anyB.matcher(content);
            StringBuilder sb = new StringBuilder(content.length());
            int last = 0;
            while (m.find()) {
                sb.append(op.apply(content.substring(last, m.start()))).append(m.group());
                last = m.end();
            }
            return sb.append(op.apply(content.substring(last))).toString();
        }

        static PrefixMap parse(String spec) {
            String[] parts = spec.split("=", -1);
            if (parts.length != 2) {
                throw new IllegalArgumentException("--map erwartet <praefixA>=<praefixB>: " + spec);
            }
            String a = parts[0].trim();
            String b = parts[1].trim();
            if (!QUALIFIED_NAME.matcher(a).matches() || !QUALIFIED_NAME.matcher(b).matches()) {
                throw new IllegalArgumentException("--map: kein gueltiger Paketname: " + spec);
            }
            if (a.equals(b) || (a + ".").startsWith(b + ".") || (b + ".").startsWith(a + ".")) {
                throw new IllegalArgumentException("--map: die Praefixe duerfen nicht gleich oder ineinander"
                        + " enthalten sein: " + spec);
            }
            return new PrefixMap(a, b);
        }

        boolean isIdentity() {
            return a.isEmpty();
        }

        PrefixMap reversed() {
            return isIdentity() ? this : new PrefixMap(b, a);
        }

        String spec() {
            return isIdentity() ? "" : a + "=" + b;
        }

        String toB(String rel) {
            return isIdentity() ? rel : pathA.matcher(rel).replaceAll(Matcher.quoteReplacement(slash(b)));
        }

        String toA(String rel) {
            return isIdentity() ? rel : pathB.matcher(rel).replaceAll(Matcher.quoteReplacement(slash(a)));
        }

        boolean underA(String rel) {
            return !isIdentity() && pathA.matcher(rel).find();
        }

        boolean underB(String rel) {
            return !isIdentity() && pathB.matcher(rel).find();
        }

        String javaToB(String content) {
            return replace(content, javaA, b);
        }

        String javaToA(String content) {
            return replace(content, javaB, a);
        }

        boolean containsB(String content) {
            if (isIdentity()) {
                return false;
            }
            for (Pattern p : javaB) {
                if (p.matcher(content).find()) {
                    return true;
                }
            }
            return false;
        }

        private static String replace(String content, Pattern[] patterns, String to) {
            if (patterns == null) {
                return content;
            }
            String result = patterns[0].matcher(content).replaceAll(Matcher.quoteReplacement(to));
            for (int i = 1; i < patterns.length; i++) {
                result = patterns[i].matcher(result).replaceAll(Matcher.quoteReplacement(slash(to)));
            }
            return result;
        }

        private static String slash(String prefix) {
            return prefix.replace('.', '/');
        }

        /** Segmentgebunden: "net/abit" trifft "src/net/abit/X.java", nicht "src/xnet/abit". */
        private static Pattern pathPattern(String prefix) {
            return prefix.isEmpty() ? null
                    : Pattern.compile("(?<![^/])" + Pattern.quote(slash(prefix)) + "(?![^/])", Pattern.CASE_INSENSITIVE);
        }

        /**
         * [0] Punktform (package, import, FQN), [1] Slashform (Ressourcenpfade), [2] Slashform direkt am Anfang
         * eines Stringliterals mit fuehrendem Slash ({@code "/net/abit/...}); andere Vorkommen mit "/" davor
         * bleiben unangetastet.
         */
        private static Pattern[] javaPatterns(String prefix) {
            return prefix.isEmpty() ? null : new Pattern[] {
                Pattern.compile("(?<![\\w.$])" + Pattern.quote(prefix) + "(?![\\w$])"),
                Pattern.compile("(?<![\\w/$])" + Pattern.quote(slash(prefix)) + "(?![\\w$])"),
                Pattern.compile("(?<=\"/)" + Pattern.quote(slash(prefix)) + "(?![\\w$])")
            };
        }
    }

    /** Woertliche Textersetzung von A nach B in Textdateien; {@link #IDENTITY}, wenn nicht angegeben. */
    static final class Replacement {
        static final Replacement IDENTITY = new Replacement("", "");

        final String a;
        final String b;
        /** a und b als UTF-8-Bytes in Latin1-Sicht, passend zu {@link Mirror#readLatin1}. */
        private final String bytesA;
        private final String bytesB;

        Replacement(String a, String b) {
            this.a = a;
            this.b = b;
            this.bytesA = latin1View(a);
            this.bytesB = latin1View(b);
        }

        static Replacement parse(String spec) {
            String[] parts = spec.split("=", -1);
            if (parts.length != 2) {
                throw new IllegalArgumentException("--replace erwartet <textA>=<textB>: " + spec);
            }
            String a = parts[0];
            String b = parts[1];
            if (a.isEmpty() || b.isEmpty() || a.equals(b)) {
                throw new IllegalArgumentException("--replace: beide Texte muessen angegeben und verschieden sein: "
                        + spec);
            }
            if (spec.contains("\t") || spec.contains("\n") || spec.contains("\r")) {
                throw new IllegalArgumentException("--replace: Tabulator und Zeilenumbruch sind nicht erlaubt: " + spec);
            }
            return new Replacement(a, b);
        }

        boolean isIdentity() {
            return a.isEmpty();
        }

        Replacement reversed() {
            return isIdentity() ? this : new Replacement(b, a);
        }

        String spec() {
            return isIdentity() ? "" : a + "=" + b;
        }

        String toB(String content) {
            return isIdentity() ? content : content.replace(bytesA, bytesB);
        }

        String toA(String content) {
            return isIdentity() ? content : content.replace(bytesB, bytesA);
        }

        boolean containsA(String content) {
            return !isIdentity() && content.contains(bytesA);
        }

        /** Ersetzt nur im Dateinamen (letztes Segment von rel), nicht in Ordnernamen. */
        String nameToB(String rel) {
            return replaceInName(rel, a, b);
        }

        String nameToA(String rel) {
            return replaceInName(rel, b, a);
        }

        private String replaceInName(String rel, String from, String to) {
            if (isIdentity()) {
                return rel;
            }
            int start = rel.lastIndexOf('/') + 1;
            return rel.substring(0, start) + rel.substring(start).replace(from, to);
        }

        private static String latin1View(String s) {
            return new String(s.getBytes(StandardCharsets.UTF_8), StandardCharsets.ISO_8859_1);
        }
    }

    /**
     * Bis zu {@link #MAX} Textersetzungen: A-&gt;B in der angegebenen Reihenfolge, B-&gt;A in umgekehrter Reihenfolge.
     * Jede Ersetzung wirkt nur ausserhalb der B-Praefixe von --map ({@link PrefixMap#outsideB}).
     */
    static final class Replacements {
        static final int MAX = 12;
        static final Replacements NONE = new Replacements(List.of());

        final List<Replacement> list;

        Replacements(List<Replacement> list) {
            this.list = List.copyOf(list);
        }

        /** Haengt eine Ersetzung an; Abbruch bei mehr als {@link #MAX} oder gleichem Text auf einer Seite. */
        Replacements plus(Replacement r) {
            if (list.size() >= MAX) {
                throw new IllegalArgumentException("--replace darf hoechstens " + MAX + "-mal angegeben werden");
            }
            for (Replacement o : list) {
                if (o.a.equals(r.a) || o.b.equals(r.b)) {
                    throw new IllegalArgumentException("--replace: " + o.spec() + " und " + r.spec()
                            + " ersetzen denselben Text");
                }
            }
            List<Replacement> l = new ArrayList<>(list);
            l.add(r);
            return new Replacements(l);
        }

        boolean isIdentity() {
            return list.isEmpty();
        }

        Replacements reversed() {
            List<Replacement> l = new ArrayList<>();
            for (Replacement r : list) {
                l.add(0, r.reversed());
            }
            return new Replacements(l);
        }

        /** Fuer Kopfzeile und Vergleich mit dem Abgleichstand: Paare durch Tabulator getrennt. */
        String spec() {
            List<String> specs = new ArrayList<>();
            list.forEach(r -> specs.add(r.spec()));
            return String.join("\t", specs);
        }

        String toB(String content, PrefixMap map) {
            String c = content;
            for (Replacement r : list) {
                c = map.outsideB(c, r::toB);
            }
            return c;
        }

        String toA(String content, PrefixMap map) {
            String c = content;
            for (int i = list.size() - 1; i >= 0; i--) {
                c = map.outsideB(c, list.get(i)::toA);
            }
            return c;
        }

        String nameToB(String rel) {
            String p = rel;
            for (Replacement r : list) {
                p = r.nameToB(p);
            }
            return p;
        }

        String nameToA(String rel) {
            String p = rel;
            for (int i = list.size() - 1; i >= 0; i--) {
                p = list.get(i).nameToA(p);
            }
            return p;
        }

        /** Die erste Ersetzung, deren A-Text im Inhalt vorkommt, sonst {@code null}. */
        Replacement findA(String content) {
            for (Replacement r : list) {
                if (r.containsA(content)) {
                    return r;
                }
            }
            return null;
        }
    }

    private final Map<Side, Path> roots = new EnumMap<>(Side.class);
    private final PrefixMap map;
    private final Replacements replace;
    private final boolean remap;
    private final boolean dryRun;
    private final boolean verbose;
    private final List<String> warnings = Collections.synchronizedList(new ArrayList<>());
    private String storedMap = "";
    private String storedReplace = "";
    private final List<IgnoreRule> ignoreRules = new ArrayList<>();
    private final Map<Side, Map<String, FileInfo>> scans = new EnumMap<>(Side.class);
    private final Set<String> unreadable = ConcurrentHashMap.newKeySet();
    private final List<String> errors = Collections.synchronizedList(new ArrayList<>());
    private final String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"));
    private Map<String, Entry> state;
    private boolean initialRun;

    Mirror(Path a, Path b, PrefixMap map, Replacements replace, boolean remap, boolean dryRun, boolean verbose) {
        roots.put(Side.A, a);
        roots.put(Side.B, b);
        this.map = map;
        this.replace = replace;
        this.remap = remap;
        this.dryRun = dryRun;
        this.verbose = verbose;
    }

    public static void main(String[] args) {
        try {
            System.exit(run(args));
        } catch (Exception e) {
            System.err.println("Abbruch: " + e);
            e.printStackTrace();
            System.exit(2);
        }
    }

    static int run(String[] args) throws Exception {
        boolean dryRun = false;
        boolean verbose = false;
        boolean remap = false;
        PrefixMap map = PrefixMap.IDENTITY;
        Replacements replace = Replacements.NONE;
        List<String> positional = new ArrayList<>();
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            switch (arg) {
                case "--dry-run" -> dryRun = true;
                case "--verbose", "-v" -> verbose = true;
                case "--remap" -> remap = true;
                case "--map" -> {
                    if (i + 1 >= args.length) {
                        return usage("--map ohne Wert");
                    }
                    try {
                        map = PrefixMap.parse(args[++i]);
                    } catch (IllegalArgumentException e) {
                        return usage(e.getMessage());
                    }
                }
                case "--replace" -> {
                    if (i + 1 >= args.length) {
                        return usage("--replace ohne Wert");
                    }
                    try {
                        replace = replace.plus(Replacement.parse(args[++i]));
                    } catch (IllegalArgumentException e) {
                        return usage(e.getMessage());
                    }
                }
                default -> {
                    if (arg.startsWith("-")) {
                        return usage("Unbekannte Option: " + arg);
                    }
                    positional.add(arg);
                }
            }
        }
        if (positional.size() != 3 || !positional.get(0).equals("sync")) {
            return usage(null);
        }
        Path a = canonical(positional.get(1));
        Path b = canonical(positional.get(2));
        if (!Files.isDirectory(a) && !Files.isDirectory(b)) {
            return usage("Keines der beiden Verzeichnisse existiert.");
        }
        if ((Files.exists(a) && !Files.isDirectory(a)) || (Files.exists(b) && !Files.isDirectory(b))) {
            return usage("Beide Pfade muessen Verzeichnisse sein.");
        }
        if (a.startsWith(b) || b.startsWith(a)) {
            return usage("Ein Verzeichnis darf nicht im anderen liegen.");
        }
        // feste Reihenfolge, damit "sync X Y" und "sync Y X" denselben Abgleichstand verwenden
        if (a.toString().compareToIgnoreCase(b.toString()) > 0) {
            Path t = a;
            a = b;
            b = t;
            map = map.reversed();
            replace = replace.reversed();
        }
        return new Mirror(a, b, map, replace, remap, dryRun, verbose).sync();
    }

    static String describeMapping(String mapSpec, String replaceSpec) {
        String m = mapSpec.isEmpty() ? "ohne --map" : "mit --map " + mapSpec;
        return replaceSpec.isEmpty() ? m : m + " und --replace " + replaceSpec.replace("\t", " --replace ");
    }

    static int usage(String message) {
        if (message != null) {
            System.err.println(message);
        }
        System.err.println(USAGE);
        return 2;
    }

    static Path canonical(String s) throws IOException {
        Path p = Path.of(s).toAbsolutePath().normalize();
        return Files.exists(p) ? p.toRealPath() : p;
    }

    int sync() throws Exception {
        long t0 = System.nanoTime();
        System.out.println("Seite A: " + roots.get(Side.A));
        System.out.println("Seite B: " + roots.get(Side.B));
        if (dryRun) {
            System.out.println("*** DRY-RUN: es wird nichts veraendert ***");
        }
        loadIgnoreRules();
        Path stateFile = stateFile();
        initialRun = !Files.isRegularFile(stateFile);
        state = loadState(stateFile);
        if (!map.isIdentity()) {
            System.out.println("Paketabbildung: A " + map.a + " <-> B " + map.b);
        }
        for (Replacement r : replace.list) {
            System.out.println("Textersetzung: A \"" + r.a + "\" <-> B \"" + r.b + "\"");
        }
        boolean replaceChanged = !initialRun && !storedReplace.equals(replace.spec());
        if (!initialRun && (!storedMap.equals(map.spec()) || replaceChanged)) {
            String stored = describeMapping(storedMap, storedReplace);
            String now = describeMapping(map.spec(), replace.spec());
            if (!remap) {
                System.err.println("Abbruch: der Abgleichstand wurde " + stored + " erstellt, dieser Aufruf ist " + now
                        + ". Ist die Aenderung gewollt, zusaetzlich --remap angeben.");
                return 2;
            }
            System.out.println("Abbildung geaendert (" + stored + " -> " + now + ").");
        }
        if (initialRun) {
            System.out.println("Erstkopie: Eclipse-Dateien (.project, .classpath, .factorypath, .settings/) werden"
                    + " einmalig mitkopiert, spaeter nicht mehr abgeglichen.");
        }

        int threads = Math.max(4, Runtime.getRuntime().availableProcessors());
        ExecutorService pool = Executors.newFixedThreadPool(threads);
        try {
            Future<Map<String, FileInfo>> fa = pool.submit(() -> scan(Side.A));
            Future<Map<String, FileInfo>> fb = pool.submit(() -> scan(Side.B));
            scans.put(Side.A, fa.get());
            scans.put(Side.B, fb.get());
            System.out.printf("Scan: A %d Dateien, B %d Dateien, Abgleichstand %d Eintraege (%s)%n",
                    scans.get(Side.A).size(), scans.get(Side.B).size(), state.size(), elapsed(t0));
            if (state.isEmpty() && !scans.get(Side.A).isEmpty() && !scans.get(Side.B).isEmpty()) {
                System.out.println("Hinweis: kein Abgleichstand vorhanden - Erstabgleich, es wird nichts geloescht;"
                        + " abweichende Dateien entscheidet die neuere Aenderungszeit.");
            }
            if (replaceChanged) {
                reportUnreplaced();
            }

            List<Action> actions = decideAll(pool);
            System.out.printf("Analyse: %d Aktionen (%s)%n", actions.size(), elapsed(t0));

            if (dryRun) {
                actions.stream().map(this::describe).filter(s -> s != null).forEach(System.out::println);
                printSummary(actions, Set.of());
                printErrors();
                return errors.isEmpty() && warnings.isEmpty() ? 0 : 1;
            }

            ensureGitExclude();
            Set<Action> failed = execute(pool, actions);
            removeEmptyDirs(actions, failed);
            saveState(stateFile);
            printSummary(actions, failed);
            printChangedFiles(actions, failed);
            printErrors();
            boolean touchedFiles = actions.stream().anyMatch(x -> x.kind() == Kind.COPY || x.kind() == Kind.DELETE);
            if (touchedFiles) {
                System.out.println("Eclipse: betroffene Projekte aktualisieren (F5 bzw. File > Refresh).");
            }
            System.out.printf("Fertig (%s)%n", elapsed(t0));
            return errors.isEmpty() && warnings.isEmpty() ? 0 : 1;
        } finally {
            pool.shutdownNow();
        }
    }

    // ---------------------------------------------------------------- Ausschluesse

    void loadIgnoreRules() throws IOException {
        for (Side side : Side.values()) {
            Path file = roots.get(side).resolve(IGNORE_FILE);
            if (!Files.isRegularFile(file)) {
                continue;
            }
            for (String line : Files.readAllLines(file, StandardCharsets.UTF_8)) {
                String rule = line.trim().replace('\\', '/');
                if (rule.isEmpty() || rule.startsWith("#")) {
                    continue;
                }
                boolean anchored = rule.startsWith("/");
                if (anchored) {
                    rule = rule.substring(1);
                }
                boolean dirOnly = rule.endsWith("/");
                if (dirOnly) {
                    rule = rule.substring(0, rule.length() - 1);
                }
                if (rule.isEmpty()) {
                    continue;
                }
                boolean matchName = !anchored && !rule.contains("/");
                ignoreRules.add(new IgnoreRule(globToRegex(rule), dirOnly, matchName));
            }
        }
    }

    static Pattern globToRegex(String glob) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < glob.length(); i++) {
            char c = glob.charAt(i);
            if (c == '*') {
                if (i + 1 < glob.length() && glob.charAt(i + 1) == '*') {
                    i++;
                    if (i + 1 < glob.length() && glob.charAt(i + 1) == '/') {
                        i++;
                        sb.append("(?:.*/)?");
                    } else {
                        sb.append(".*");
                    }
                } else {
                    sb.append("[^/]*");
                }
            } else if (c == '?') {
                sb.append("[^/]");
            } else {
                sb.append(Pattern.quote(String.valueOf(c)));
            }
        }
        return Pattern.compile(sb.toString(), Pattern.CASE_INSENSITIVE);
    }

    boolean isExcluded(String rel, String name, boolean isDir) {
        if (isDir && DEFAULT_EXCLUDED_DIRS.contains(name.toLowerCase(Locale.ROOT)) && !(name.equals("api") && !rel.contains("docs"))) {
            return true;
        }
        if (!isDir && name.endsWith(TMP_SUFFIX)) {
            return true;
        }
        if (!isDir && isExcludedFile(rel)) {
            return true;
        }
        for (IgnoreRule rule : ignoreRules) {
            if (rule.dirOnly() && !isDir) {
                continue;
            }
            if (rule.pattern().matcher(rule.matchName() ? name : rel).matches()) {
                return true;
            }
        }
        return false;
    }

    /** {@link #EXCLUDE_FILES} gilt fuer beide Seiten: Pfad in A- und in B-Form pruefen. */
    boolean isExcludedFile(String rel) {
        return EXCLUDE_FILES.matcher(rel).matches()
                || EXCLUDE_FILES.matcher(relToA(rel)).matches()
                || EXCLUDE_FILES.matcher(relToB(rel)).matches();
    }

    // ---------------------------------------------------------------- Scan

    Map<String, FileInfo> scan(Side side) throws IOException {
        Path root = roots.get(side);
        Map<String, FileInfo> result = new HashMap<>();
        if (!Files.isDirectory(root)) {
            return result;
        }
        Files.walkFileTree(root, new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                if (dir.equals(root)) {
                    return FileVisitResult.CONTINUE;
                }
                return isExcluded(rel(root, dir), dir.getFileName().toString(), true)
                        ? FileVisitResult.SKIP_SUBTREE
                        : FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                if (attrs.isRegularFile()) {
                    String rel = rel(root, file);
                    if (!isExcluded(rel, file.getFileName().toString(), false)) {
                        String key = key(side, rel);
                        if (key != null && result.put(key, new FileInfo(attrs.size(),
                                attrs.lastModifiedTime().toMillis())) != null) {
                            block(key, "Scan " + side + ": " + rel + " kollidiert nach Paketabbildung mit " + key);
                        }
                    }
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFileFailed(Path file, IOException exc) {
                // nicht lesbar: alles darunter bleibt unangetastet, sonst saehe es wie eine Loeschung aus
                String rel = rel(root, file);
                block(side == Side.B ? relToA(rel) : rel, "Scan " + side + ": " + rel + ": " + exc);
                return FileVisitResult.CONTINUE;
            }
        });
        return result;
    }

    /**
     * Schluessel im Abgleichstand (A-Form). Liegt eine Datei unter dem Praefixpfad der jeweils anderen Seite
     * (z. B. B noch nicht vollstaendig umbenannt) oder ist sein Dateiname nicht eindeutig per --replace
     * umrechenbar, wird der Pfad gesperrt und {@code null} geliefert.
     */
    String key(Side side, String rel) {
        if (replacesName(rel)) {
            boolean reversible = side == Side.A
                    ? replace.nameToA(replace.nameToB(rel)).equals(rel)
                    : replace.nameToB(replace.nameToA(rel)).equals(rel);
            if (!reversible) {
                // auch das Gegenstueck auf der anderen Seite sperren, sonst saehe es wie eine Loeschung aus
                block(side == Side.B ? relToA(rel) : rel, "Scan " + side + ": Dateiname " + rel
                        + " passt nicht zu --replace (enthaelt den Text der anderen Seite oder ist noch nicht"
                        + " umbenannt)");
                return null;
            }
        }
        if (!isMapped(rel)) {
            return side == Side.B ? relToA(rel) : rel;
        }
        if (side == Side.A) {
            if (map.underB(rel)) {
                block(rel, "Scan A: " + rel + " liegt unter dem B-Praefix " + map.b);
                return null;
            }
            return rel;
        }
        if (map.underA(rel)) {
            block(rel, "Scan B: " + rel + " liegt noch unter dem A-Praefix " + map.a + " (Umbenennung unvollstaendig?)");
            return null;
        }
        return relToA(rel);
    }

    /** Pfad (und alles darunter) fuer diesen Lauf von jeder Aktion ausnehmen. */
    void block(String key, String message) {
        unreadable.add(key);
        errors.add(message);
    }

    /** Eclipse-Konfiguration: nur bei der Erstkopie gespiegelt, nie im Abgleichstand. */
    static boolean isEclipseConfig(String rel) {
        String[] segments = rel.toLowerCase(Locale.ROOT).split("/");
        if (ECLIPSE_FILES.contains(segments[segments.length - 1])) {
            return true;
        }
        for (int i = 0; i < segments.length - 1; i++) {
            if (segments[i].equals(ECLIPSE_SETTINGS_DIR)) {
                return true;
            }
        }
        return false;
    }

    static String rel(Path root, Path p) {
        return root.relativize(p).toString().replace('\\', '/');
    }

    boolean isUnreadable(String rel) {
        if (unreadable.isEmpty()) {
            return false;
        }
        for (String p = rel;;) {
            if (unreadable.contains(p)) {
                return true;
            }
            int slash = p.lastIndexOf('/');
            if (slash < 0) {
                return false;
            }
            p = p.substring(0, slash);
        }
    }

    // ---------------------------------------------------------------- Entscheidung

    List<Action> decideAll(ExecutorService pool) throws InterruptedException {
        Set<String> all = new TreeSet<>(state.keySet());
        all.addAll(scans.get(Side.A).keySet());
        all.addAll(scans.get(Side.B).keySet());
        List<Callable<Action>> tasks = new ArrayList<>(all.size());
        for (String rel : all) {
            tasks.add(() -> {
                if (isUnreadable(rel)) {
                    return null;
                }
                try {
                    return decide(rel);
                } catch (IOException e) {
                    errors.add("Analyse " + rel + ": " + e);
                    return null;
                }
            });
        }
        List<Action> actions = new ArrayList<>();
        for (Future<Action> f : pool.invokeAll(tasks)) {
            try {
                Action a = f.get();
                if (a != null) {
                    actions.add(a);
                }
            } catch (ExecutionException e) {
                errors.add("Analyse: " + e.getCause());
            }
        }
        return actions;
    }

    /**
     * Nur melden: Textdateien, die vor Einfuehrung von --replace 1:1 nach B kopiert wurden und den A-Text noch
     * enthalten. Ihr Hash stimmt mit A ueberein, deshalb wuerden sie sonst nie angeglichen.
     */
    void reportUnreplaced() {
        if (replace.isIdentity()) {
            return;
        }
        for (String rel : new TreeSet<>(state.keySet())) {
            if (!replacesContent(rel) || isUnreadable(rel)
                    || !scans.get(Side.A).containsKey(rel) || !scans.get(Side.B).containsKey(rel)) {
                continue;
            }
            try {
                Replacement r = replace.findA(readLatin1(path(Side.B, rel)));
                if (r != null) {
                    warnings.add(relToB(rel) + ": enthaelt noch \"" + r.a
                            + "\" (vor --replace 1:1 kopiert), bitte angleichen");
                }
            } catch (IOException e) {
                errors.add("Pruefung " + rel + ": " + e);
            }
        }
    }

    Action decide(String rel) throws IOException {
        Action action = decideRaw(rel);
        if (action != null && action.kind() == Kind.COPY && action.side() == Side.B && isMapped(rel)
                && !isJavaOrJSon(rel) && map.containsB(replaceToA(rel, readLatin1(path(Side.B, rel))))) {
            warnings.add(relToB(rel) + ": enthaelt B-Praefix " + map.b + ", nicht nach A uebertragen");
            return null;
        }
        return action;
    }

    Action decideRaw(String rel) throws IOException {
        FileInfo a = scans.get(Side.A).get(rel);
        FileInfo b = scans.get(Side.B).get(rel);
        Entry e = state.get(rel);

        if (isEclipseConfig(rel)) {
            if (initialRun && a == null) {
                return new Action(Kind.COPY, rel, Side.B, false, null, null);
            }
            if (initialRun && b == null) {
                return new Action(Kind.COPY, rel, Side.A, false, null, null);
            }
            return e != null ? new Action(Kind.FORGET, rel, null, false, null, null) : null;
        }

        if (e == null) {
            if (a == null) {
                return new Action(Kind.COPY, rel, Side.B, false, null, null);
            }
            if (b == null) {
                return new Action(Kind.COPY, rel, Side.A, false, null, null);
            }
            String ha = hashOf(Side.A, rel);
            String hb = hashOf(Side.B, rel);
            if (ha.equals(hb)) {
                return new Action(Kind.RECORD, rel, null, false, ha, null);
            }
            return conflict(rel, a, b, "Konflikt ohne Abgleichstand");
        }

        Observed oa = observe(Side.A, rel, a, e);
        Observed ob = observe(Side.B, rel, b, e);
        Status sa = oa.status();
        Status sb = ob.status();

        if (sa == Status.DELETED && sb == Status.DELETED) {
            return new Action(Kind.FORGET, rel, null, false, null, null);
        }
        if (sa == Status.DELETED || sb == Status.DELETED) {
            Side gone = sa == Status.DELETED ? Side.A : Side.B;
            Status remaining = gone == Side.A ? sb : sa;
            if (remaining == Status.CHANGED) {
                return new Action(Kind.COPY, rel, gone.other(), false, null,
                        "Loeschung auf " + gone + " verworfen, da auf " + gone.other()
                                + " geaendert - Datei wiederhergestellt");
            }
            return new Action(Kind.DELETE, rel, gone.other(), true, null, null);
        }
        boolean changedA = sa == Status.CHANGED;
        boolean changedB = sb == Status.CHANGED;
        if (changedA && changedB) {
            if (oa.hash().equals(ob.hash())) {
                return new Action(Kind.RECORD, rel, null, false, oa.hash(), null);
            }
            return conflict(rel, a, b, "Konflikt");
        }
        if (changedA) {
            return new Action(Kind.COPY, rel, Side.A, false, null, null);
        }
        if (changedB) {
            return new Action(Kind.COPY, rel, Side.B, false, null, null);
        }
        if (sa == Status.TOUCHED || sb == Status.TOUCHED) {
            return new Action(Kind.RECORD, rel, null, false, e.hash(), null);
        }
        return null;
    }

    Observed observe(Side side, String rel, FileInfo info, Entry e) throws IOException {
        if (info == null) {
            return new Observed(Status.DELETED, null);
        }
        if (info.size() == e.size(side) && info.mtime() == e.mtime(side)) {
            return new Observed(Status.UNCHANGED, e.hash());
        }
        String h = hashOf(side, rel);
        return new Observed(h.equals(e.hash()) ? Status.TOUCHED : Status.CHANGED, h);
    }

    static Action conflict(String rel, FileInfo a, FileInfo b, String what) {
        Side winner = a.mtime() >= b.mtime() ? Side.A : Side.B;
        return new Action(Kind.COPY, rel, winner, true, null,
                what + ": " + winner + " ist neuer und gewinnt, Version von " + winner.other() + " gesichert");
    }

    // ---------------------------------------------------------------- Ausfuehrung

    Set<Action> execute(ExecutorService pool, List<Action> actions) throws InterruptedException {
        Set<Action> failed = ConcurrentHashMap.newKeySet();
        AtomicInteger done = new AtomicInteger();
        int total = actions.size();
        List<Callable<Void>> tasks = new ArrayList<>(total);
        for (Action action : actions) {
            tasks.add(() -> {
                try {
                    apply(action);
                } catch (IOException | RuntimeException e) {
                    failed.add(action);
                    errors.add(action.kind() + " " + action.rel() + ": " + e);
                }
                int n = done.incrementAndGet();
                if (total >= 5000 && n % 5000 == 0) {
                    System.out.printf("  ... %d / %d%n", n, total);
                }
                return null;
            });
        }
        pool.invokeAll(tasks);
        if (verbose) {
            actions.stream().filter(a -> !failed.contains(a)).map(this::describe).filter(s -> s != null)
                    .forEach(System.out::println);
        } else {
            actions.stream().filter(a -> a.note() != null && !failed.contains(a)).map(this::describe)
                    .forEach(System.out::println);
        }
        return failed;
    }

    void apply(Action action) throws IOException {
        String rel = action.rel();
        switch (action.kind()) {
            case COPY -> {
                Side src = action.side();
                Side dst = src.other();
                Path target = path(dst, rel);
                if (action.backup() && Files.exists(target)) {
                    backup(dst, rel);
                }
                String h = rewrites(rel) ? copyRewritten(src, rel) : copy(path(src, rel), target);
                if (isEclipseConfig(rel)) {
                    state.remove(rel);
                } else {
                    FileInfo srcInfo = stat(path(src, rel));
                    FileInfo dstInfo = stat(target);
                    state.put(rel, src == Side.A ? Entry.of(h, srcInfo, dstInfo) : Entry.of(h, dstInfo, srcInfo));
                }
            }
            case DELETE -> {
                if (Files.exists(path(action.side(), rel))) {
                    backup(action.side(), rel);
                }
                state.remove(rel);
            }
            case RECORD -> state.put(rel, Entry.of(action.hash(),
                    scans.get(Side.A).get(rel), scans.get(Side.B).get(rel)));
            case FORGET -> state.remove(rel);
        }
    }

    static String copy(Path src, Path dst) throws IOException {
        Files.createDirectories(dst.getParent());
        Path tmp = dst.resolveSibling(dst.getFileName() + TMP_SUFFIX);
        MessageDigest md = sha1();
        try {
            try (InputStream in = new DigestInputStream(Files.newInputStream(src), md)) {
                Files.copy(in, tmp, StandardCopyOption.REPLACE_EXISTING);
            }
            Files.setLastModifiedTime(tmp, Files.getLastModifiedTime(src));
            clearReadOnly(dst);
            Files.move(tmp, dst, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } finally {
            Files.deleteIfExists(tmp);
        }
        return HexFormat.of().formatHex(md.digest());
    }

    /** Kopiert eine Datei mit Paket-Umschreibung und/oder Textersetzung; liefert den Hash der A-Form. */
    String copyRewritten(Side src, String rel) throws IOException {
        Path from = path(src, rel);
        Path to = path(src.other(), rel);
        String content = readLatin1(from);
        String converted = src == Side.A ? contentToB(rel, content) : contentToA(rel, content);
        String back = src == Side.A ? contentToA(rel, converted) : contentToB(rel, converted);
        if (!back.equals(content)) {
            throw new IOException("Umschreibung nicht eindeutig umkehrbar (Datei enthaelt bereits das Praefix bzw."
                    + " den Ersatztext der Zielseite) - nicht kopiert");
        }
        Files.createDirectories(to.getParent());
        Path tmp = to.resolveSibling(to.getFileName() + TMP_SUFFIX);
        try {
            Files.write(tmp, converted.getBytes(StandardCharsets.ISO_8859_1));
            Files.setLastModifiedTime(tmp, Files.getLastModifiedTime(from));
            clearReadOnly(to);
            Files.move(tmp, to, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } finally {
            Files.deleteIfExists(tmp);
        }
        return sha1Hex(src == Side.A ? content : converted);
    }

    void backup(Side side, String rel) throws IOException {
        Path file = path(side, rel);
        Path target = roots.get(side).resolve(".mirror").resolve("backup").resolve(timestamp)
                .resolve(roots.get(side).relativize(file));
        Files.createDirectories(target.getParent());
        clearReadOnly(file);
        Files.move(file, target, StandardCopyOption.REPLACE_EXISTING);
    }

    static void clearReadOnly(Path p) {
        if (Files.exists(p) && !Files.isWritable(p)) {
            try {
                Files.setAttribute(p, "dos:readonly", false);
            } catch (IOException | UnsupportedOperationException ignored) {
                // der folgende move meldet den eigentlichen Fehler
            }
        }
    }

    /** Entfernt Verzeichnisse, die durch propagierte Loeschungen leer geworden sind. */
    void removeEmptyDirs(List<Action> actions, Set<Action> failed) {
        for (Side side : Side.values()) {
            Path root = roots.get(side);
            Set<Path> dirs = new HashSet<>();
            for (Action a : actions) {
                if (a.kind() == Kind.DELETE && a.side() == side && !failed.contains(a)) {
                    for (Path p = path(side, a.rel()).getParent(); p != null && !p.equals(root); p = p.getParent()) {
                        dirs.add(p);
                    }
                }
            }
            dirs.stream().sorted(Comparator.comparingInt(Path::getNameCount).reversed()).forEach(dir -> {
                try {
                    Files.deleteIfExists(dir);
                } catch (DirectoryNotEmptyException ignored) {
                    // noch Inhalt vorhanden
                } catch (IOException e) {
                    errors.add("Verzeichnis " + dir + ": " + e);
                }
            });
        }
    }

    /** Traegt .mirror/ in .git/info/exclude ein, damit Backups nicht in git status auftauchen. */
    void ensureGitExclude() {
        for (Side side : Side.values()) {
            Path gitDir = roots.get(side).resolve(".git");
            if (!Files.isDirectory(gitDir)) {
                continue;
            }
            Path exclude = gitDir.resolve("info").resolve("exclude");
            try {
                if (Files.isRegularFile(exclude)
                        && Files.readAllLines(exclude, StandardCharsets.UTF_8).stream()
                                .anyMatch(l -> l.trim().equals(GIT_EXCLUDE_LINE))) {
                    continue;
                }
                Files.createDirectories(exclude.getParent());
                Files.writeString(exclude, System.lineSeparator() + GIT_EXCLUDE_LINE + System.lineSeparator(),
                        StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.APPEND);
                System.out.println("Eingetragen: " + GIT_EXCLUDE_LINE + " in " + exclude);
            } catch (IOException e) {
                errors.add("git exclude " + exclude + ": " + e);
            }
        }
    }

    // ---------------------------------------------------------------- State

    Path stateFile() {
        String key = HexFormat.of().formatHex(
                sha1().digest((roots.get(Side.A) + "|" + roots.get(Side.B)).getBytes(StandardCharsets.UTF_8)));
        return Path.of(System.getProperty("user.home"), ".eclipse-mirror", key.substring(0, 16) + ".tsv");
    }

    Map<String, Entry> loadState(Path file) throws IOException {
        Map<String, Entry> result = new ConcurrentHashMap<>();
        if (!Files.isRegularFile(file)) {
            return result;
        }
        List<String> lines = Files.readAllLines(file, StandardCharsets.UTF_8);
        // v1: "# eclipse-mirror v1 \t A \t B", v2 zusaetzlich "\t map=<a>=<b>",
        // v3 zusaetzlich je --replace "\t replace=<a>=<b>" (ohne --replace genau ein leeres "\t replace=")
        String[] h = lines.isEmpty() ? new String[0] : lines.get(0).split("\t", -1);
        boolean v1 = h.length == 3 && h[0].equals(STATE_HEADER_V1);
        boolean v2 = h.length == 4 && h[0].equals(STATE_HEADER_V2) && h[3].startsWith("map=");
        boolean v3 = h.length >= 5 && h.length <= 4 + Replacements.MAX && h[0].equals(STATE_HEADER)
                && h[3].startsWith("map=");
        List<String> replaceSpecs = new ArrayList<>();
        for (int i = 4; v3 && i < h.length; i++) {
            v3 = h[i].startsWith("replace=");
            String spec = v3 ? h[i].substring("replace=".length()) : "";
            if (!spec.isEmpty()) {
                replaceSpecs.add(spec);
            }
        }
        if (!(v1 || v2 || v3) || !h[1].equals(roots.get(Side.A).toString())
                || !h[2].equals(roots.get(Side.B).toString())) {
            throw new IOException("Abgleichstand " + file + " passt nicht zu den Verzeichnissen (Kopfzeile)");
        }
        storedMap = v2 || v3 ? h[3].substring("map=".length()) : "";
        storedReplace = String.join("\t", replaceSpecs);
        for (String line : lines.subList(1, lines.size())) {
            String[] f = line.split("\t", -1);
            if (f.length != 6) {
                throw new IOException("Abgleichstand " + file + " defekt: " + line);
            }
            result.put(f[0], new Entry(f[1], Long.parseLong(f[2]), Long.parseLong(f[3]),
                    Long.parseLong(f[4]), Long.parseLong(f[5])));
        }
        return result;
    }

    void saveState(Path file) throws IOException {
        Files.createDirectories(file.getParent());
        List<String> lines = new ArrayList<>(state.size() + 1);
        lines.add(header());
        new TreeMap<>(state).forEach((rel, e) -> lines.add(String.join("\t", rel, e.hash(),
                Long.toString(e.sizeA()), Long.toString(e.mtimeA()),
                Long.toString(e.sizeB()), Long.toString(e.mtimeB()))));
        Path tmp = file.resolveSibling(file.getFileName() + ".tmp");
        Files.write(tmp, lines, StandardCharsets.UTF_8);
        Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
    }

    String header() {
        return STATE_HEADER + "\t" + roots.get(Side.A) + "\t" + roots.get(Side.B) + "\tmap=" + map.spec()
                + "\treplace=" + replace.spec().replace("\t", "\treplace=");
    }

    // ---------------------------------------------------------------- Ausgabe

    String describe(Action a) {
        String line = switch (a.kind()) {
            case COPY -> a.side() + " -> " + a.side().other() + "  " + a.rel();
            case DELETE -> "DEL " + a.side() + "    " + a.rel();
            case RECORD -> verbose ? "=         " + a.rel() : null;
            case FORGET -> verbose ? "-         " + a.rel() : null;
        };
        if (line != null && a.note() != null) {
            line += "   [" + a.note() + "]";
        }
        return line;
    }

    void printSummary(List<Action> actions, Set<Action> failed) {
        int copyAB = 0;
        int copyBA = 0;
        int delA = 0;
        int delB = 0;
        int notes = 0;
        for (Action a : actions) {
            if (failed.contains(a)) {
                continue;
            }
            if (a.kind() == Kind.COPY) {
                if (a.side() == Side.A) {
                    copyAB++;
                } else {
                    copyBA++;
                }
            } else if (a.kind() == Kind.DELETE) {
                if (a.side() == Side.A) {
                    delA++;
                } else {
                    delB++;
                }
            }
            if (a.note() != null) {
                notes++;
            }
        }
        System.out.printf("%s A->B %d, B->A %d, geloescht auf A %d, auf B %d, Konflikte/Wiederherstellungen %d,"
                + " Warnungen %d, Fehler %d%n", dryRun ? "Geplant:" : "Ergebnis:", copyAB, copyBA, delA, delB, notes,
                warnings.size(), errors.size());
        if (!dryRun && (notes > 0 || delA + delB > 0)) {
            System.out.println("Gesicherte Versionen: <Seite>/.mirror/backup/" + timestamp + "/");
        }
    }

    /** Listet alle tatsaechlich geaenderten Dateien (Kopie/Loeschung) mit Aktion. */
    void printChangedFiles(List<Action> actions, Set<Action> failed) {
        List<String> lines = actions.stream()
                .filter(a -> (a.kind() == Kind.COPY || a.kind() == Kind.DELETE) && !failed.contains(a))
                .map(this::describe).toList();
        if (!lines.isEmpty()) {
            System.out.println("Geaenderte Dateien (" + lines.size() + "):");
            lines.forEach(l -> System.out.println("  " + l));
        }
    }

    void printErrors() {
        print("WARNUNG ", warnings);
        print("FEHLER ", errors);
    }

    static void print(String prefix, List<String> messages) {
        synchronized (messages) {
            messages.stream().sorted().limit(MAX_PRINTED_ERRORS).forEach(m -> System.err.println(prefix + m));
            if (messages.size() > MAX_PRINTED_ERRORS) {
                System.err.println(prefix + "... und " + (messages.size() - MAX_PRINTED_ERRORS) + " weitere");
            }
        }
    }

    // ---------------------------------------------------------------- Hilfen

    /** Physischer Pfad; {@code rel} ist der Schluessel in A-Form, auf B wird das Paket-Praefix abgebildet. */
    Path path(Side side, String rel) {
        return roots.get(side).resolve(side == Side.B ? relToB(rel) : rel);
    }

    /** Schluessel (A-Form) -&gt; Pfad in B: Paket-Praefix abbilden, dann --replace im Dateinamen. */
    String relToB(String rel) {
        String p = isMapped(rel) ? map.toB(rel) : rel;
        return replacesName(rel) ? replace.nameToB(p) : p;
    }

    /** Pfad in B -&gt; Schluessel (A-Form): genau umgekehrt zu {@link #relToB}. */
    String relToA(String rel) {
        String p = isMapped(rel) ? map.toA(rel) : rel;
        return replacesName(rel) ? replace.nameToA(p) : p;
    }

    /** --replace im Dateinamen; nicht fuer {@link #NO_MAP_DIRS}. */
    boolean replacesName(String rel) {
        return !replace.isIdentity() && !inNoMapDir(rel);
    }

    /** Gilt die Paketabbildung fuer diesen Pfad? Nicht fuer {@link #NO_MAP_DIRS}. */
    boolean isMapped(String rel) {
        return !map.isIdentity() && !inNoMapDir(rel);
    }

    static boolean inNoMapDir(String rel) {
        String lower = rel.toLowerCase(Locale.ROOT);
        for (String dir : NO_MAP_DIRS) {
            if (lower.equals(dir) || lower.startsWith(dir + "/")) {
                return true;
            }
        }
        return false;
    }

    boolean rewrites(String rel) {
        return mapsContent(rel) || replacesContent(rel);
    }

    /** Paket-Umschreibung des Inhalts (.java/.json). */
    boolean mapsContent(String rel) {
        return isMapped(rel) && isJavaOrJSon(rel);
    }

    /** Textersetzung (--replace) des Inhalts. */
    boolean replacesContent(String rel) {
        return !replace.isIdentity() && !inNoMapDir(rel) && isText(rel);
    }

    static boolean isJavaOrJSon(String rel) {
        return rel.toLowerCase(Locale.ROOT).endsWith(".java") || rel.toLowerCase(Locale.ROOT).endsWith(".json");
    }

    static boolean isText(String rel) {
        String name = rel.substring(rel.lastIndexOf('/') + 1);
        int dot = name.lastIndexOf('.');
        return dot >= 0 && TEXT_EXTENSIONS.contains(name.substring(dot + 1).toLowerCase(Locale.ROOT));
    }

    /** A-Form -&gt; B-Form: erst Paket-Umschreibung, dann Textersetzung. */
    String contentToB(String rel, String content) {
        String c = mapsContent(rel) ? map.javaToB(content) : content;
        return replacesContent(rel) ? replace.toB(c, map) : c;
    }

    /** B-Form -&gt; A-Form: genau umgekehrte Reihenfolge von {@link #contentToB}. */
    String contentToA(String rel, String content) {
        String c = replaceToA(rel, content);
        return mapsContent(rel) ? map.javaToA(c) : c;
    }

    String replaceToA(String rel, String content) {
        return replacesContent(rel) ? replace.toA(content, map) : content;
    }

    /** Hash der A-Form: umgeschriebene B-seitige Dateien werden vor dem Hashen zurueckuebersetzt. */
    String hashOf(Side side, String rel) throws IOException {
        if (side == Side.B && rewrites(rel)) {
            return sha1Hex(contentToA(rel, readLatin1(path(side, rel))));
        }
        return hash(path(side, rel));
    }

    /** ISO-8859-1 bildet jedes Byte verlustfrei auf ein Zeichen ab; die Praefixe sind reines ASCII. */
    static String readLatin1(Path p) throws IOException {
        return new String(Files.readAllBytes(p), StandardCharsets.ISO_8859_1);
    }

    static String sha1Hex(String latin1) {
        return HexFormat.of().formatHex(sha1().digest(latin1.getBytes(StandardCharsets.ISO_8859_1)));
    }

    static FileInfo stat(Path p) throws IOException {
        BasicFileAttributes attrs = Files.readAttributes(p, BasicFileAttributes.class);
        return new FileInfo(attrs.size(), attrs.lastModifiedTime().toMillis());
    }

    static String hash(Path p) throws IOException {
        MessageDigest md = sha1();
        byte[] buf = new byte[64 * 1024];
        try (InputStream in = Files.newInputStream(p)) {
            for (int n = in.read(buf); n > 0; n = in.read(buf)) {
                md.update(buf, 0, n);
            }
        } catch (AccessDeniedException e) {
            throw new IOException("Zugriff verweigert (gesperrt?): " + p, e);
        }
        return HexFormat.of().formatHex(md.digest());
    }

    static MessageDigest sha1() {
        try {
            return MessageDigest.getInstance("SHA-1");
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    static String elapsed(long t0) {
        return String.format(Locale.ROOT, "%.1f s", (System.nanoTime() - t0) / 1e9);
    }
}

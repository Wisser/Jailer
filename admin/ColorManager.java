package net.sf.jailer.color_manager;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;


public class ColorManager {

	public static void main(String[] args) throws IOException {

		Path sourcePath = new File("src/main/gui").toPath();
		Files.walkFileTree(sourcePath, new FileVisitor());
		
		PrintWriter out = new PrintWriter(new File(new File("src/main/gui/net/sf/jailer/ui"), "Colors.java"));
		out.println("package net.sf.jailer.ui;");
		out.println();
		out.println("import java.awt.Color;");
		out.println();
		out.println("public class Colors {");
		colors.forEach((key, color) -> {
			out.println("\tpublic static Color " + key + " = " + color+ ";");
		});
		out.println("}");
		out.close();
	}

	static Map<String, String> colors = new TreeMap<>();
	
	private static class FileVisitor extends SimpleFileVisitor<Path> {
		private Path sourcePath = null;

		@Override
		public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
			if (sourcePath == null) {
				sourcePath = dir;
			}
			return FileVisitResult.CONTINUE;
		}

		@Override
		public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
			if (!file.getFileName().toString().startsWith(ColorManager.class.getSimpleName())
					&& file.getFileName().toString().matches(".*\\.(java|form)")) {
				System.out.println(file);
				String content = new String(Files.readAllBytes(file));
				String result = transform(content);
				if (!result.equals(content) && !file.getFileName().toString().matches(".*\\bColors\\.java")) {
					Files.write(file, result.getBytes());
				}
			} else {
				System.out.println("skipped");
			}
			return FileVisitResult.CONTINUE;
		}

		private String transform(String content) {
			String result = content;
			
			result = find("(?:\\bnew\\s+(?:java\\.awt\\.)?Color\\s*\\((\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*(?:,\\s*(\\d+)\\s*)?\\))|(?:\\b(?:java\\.awt\\.)?Color\\b\\s*\\.\\s*\\b(\\w+)\\b)", result,
					m -> {
						String key = "Color_" + Arrays.asList(m.group(1), m.group(2), m.group(3), m.group(4), m.group(5)).stream().filter(g -> g != null).map(String::toLowerCase).collect(Collectors.joining("_"));
						System.out.println(key);
						colors.put(key, m.group());
						return "Colors." + key;
					});
			
//            <Property name="background" type="java.awt.Color" editor="org.netbeans.modules.form.RADConnectionPropertyEditor">
//            	<Connection code="Colors.Color_255_255_255_150" type="code"/>
//            </Property>
//            <Property name="foreground" type="java.awt.Color" editor="org.netbeans.beaninfo.editors.ColorEditor">
//            	<Color blue="10" green="10" red="8d" type="rgb"/>
//            </Property>

			result = find("type=\"java.awt.Color\" editor=\"org.netbeans.beaninfo.editors.ColorEditor\">", result, m -> "type=\"java.awt.Color\" editor=\"org.netbeans.modules.form.RADConnectionPropertyEditor\">");
			
			result = find("\\<Color\\b.*\\bblue=\"([^\"]*)\".*\\bgreen=\"([^\"]*)\".*\\bred=\"([^\"]*)\".*>", result,
					m -> {
						String key = "Color_" + Arrays.asList(m.group(1), m.group(2), m.group(3)).stream().map(g -> "" + Integer.parseInt(g, 16)).collect(Collectors.joining("_"));
						String color = "new Color(" + Arrays.asList(m.group(1), m.group(2), m.group(3)).stream().map(g -> "" + Integer.parseInt(g, 16)).collect(Collectors.joining(", ")) + ")";
						System.out.println("!" + key);
						colors.put(key, color);
						return "<Connection code=\"Colors." + key + "\" type=\"code\"/>";
					});

			return result;
		}
	}
	
	private static String find(String regEx, String content, Function<Matcher, String> replacement) {
		Matcher matcher = Pattern.compile(regEx).matcher(content);
		matcher.reset();
        boolean result = matcher.find();
        if (result) {
            StringBuffer sb = new StringBuffer();
            do {
            	matcher.appendReplacement(sb, "");
            	String apply = replacement.apply(matcher);
				sb.append(apply);
                result = matcher.find();
            } while (result);
            matcher.appendTail(sb);
            return sb.toString();
        }
        return content.toString();
	}
}

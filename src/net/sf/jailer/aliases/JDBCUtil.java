package net.sf.jailer.aliases;

/**
 * @author Vladimir "Dair T'arg" Berkutov
 * @date: 01.03.2008
 * @time: 22:21:08
 */
public final class JDBCUtil {

	private JDBCUtil() {}

	public static String getSubprotocol(String url)
	throws IllegalArgumentException {
		int start = url.indexOf(':');
		if (start == -1) {
			throw new IllegalArgumentException("Invalid url string");
		}
		int finish = url.indexOf(':', start + 1);
		if (finish == -1) {
			throw new IllegalArgumentException("Invalid url string");
		}
		return url.substring(start + 1, finish);
	}

	public static String implode(String strings[], String delimeter) {
		if (strings == null) {
			return "";
		}
		String result = strings[0];
		for (int i = 1; i < strings.length; i++) {
			result += delimeter + strings[i];
		}
		return result;
	}

	public static String getLibraryName(String fullName) {
		return fullName.substring(0, fullName.indexOf('#'));
	}

	public static String getClassName(String fullName) {
		return fullName.substring(fullName.indexOf("#") + 1);
	}
}

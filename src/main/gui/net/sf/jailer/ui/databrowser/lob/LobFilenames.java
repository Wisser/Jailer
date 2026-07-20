/*
 * Copyright 2007 - 2026 Ralf Wisser.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.sf.jailer.ui.databrowser.lob;

/**
 * Derives a default export file name for a {@link LobContent} from its origin
 * (table and column), sanitized for use on any file system.
 *
 * @author Ralf Wisser
 */
public final class LobFilenames {

	private LobFilenames() {
	}

	/**
	 * The base file name (without extension). Prefers the content's original
	 * file name (e.g. a ZIP entry's name) when known, falling back to
	 * {@code table_column}.
	 */
	public static String baseName(LobContent content) {
		String original = content.getOriginalFileName();
		if (original != null) {
			StringBuilder sb = new StringBuilder();
			append(sb, stripExtension(stripPath(original)));
			if (sb.length() > 0) {
				return sb.toString();
			}
		}
		StringBuilder sb = new StringBuilder();
		append(sb, content.getTable());
		append(sb, content.getColumn());
		String s = sb.toString();
		return s.isEmpty() ? "lob" : s;
	}

	private static String stripPath(String name) {
		int slash = Math.max(name.lastIndexOf('/'), name.lastIndexOf('\\'));
		return slash >= 0 ? name.substring(slash + 1) : name;
	}

	private static String stripExtension(String name) {
		int dot = name.lastIndexOf('.');
		return dot > 0 ? name.substring(0, dot) : name;
	}

	private static void append(StringBuilder sb, String part) {
		if (part == null) {
			return;
		}
		String cleaned = part.replaceAll("[^\\p{Alnum}]+", "_").replaceAll("^_+|_+$", "");
		if (cleaned.isEmpty()) {
			return;
		}
		if (sb.length() > 0) {
			sb.append("_");
		}
		sb.append(cleaned);
	}
}

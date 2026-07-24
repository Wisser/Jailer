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

import org.fife.ui.rsyntaxtextarea.SyntaxConstants;

/**
 * The kind of content guessed for a BLOB/CLOB cell value. Each constant carries
 * the display category (how the {@link LobViewerPanel} renders it), the file
 * extension used when exporting to a file, an optional RSyntaxTextArea syntax
 * style and a human readable name.
 *
 * @author Ralf Wisser
 */
public enum LobContentType {

	PNG(Category.IMAGE, ".png", null, "PNG Image"),
	JPEG(Category.IMAGE, ".jpg", null, "JPEG Image"),
	GIF(Category.IMAGE, ".gif", null, "GIF Image"),
	BMP(Category.IMAGE, ".bmp", null, "BMP Image"),
	SVG(Category.IMAGE, ".svg", null, "SVG Image"),

	PLAIN_TEXT(Category.TEXT, ".txt", SyntaxConstants.SYNTAX_STYLE_NONE, "Text"),
	JSON(Category.TEXT, ".json", SyntaxConstants.SYNTAX_STYLE_JSON_WITH_COMMENTS, "JSON"),
	XML(Category.TEXT, ".xml", SyntaxConstants.SYNTAX_STYLE_XML, "XML"),
	HTML(Category.TEXT, ".html", SyntaxConstants.SYNTAX_STYLE_HTML, "HTML"),

	PDF(Category.PDF, ".pdf", null, "PDF Doc"),

	ZIP(Category.ARCHIVE, ".zip", null, "ZIP Archive"),
	GZIP(Category.ARCHIVE, ".gz", null, "GZIP Archive"),

	// Office Open XML packages: still ARCHIVE (the original bytes are the real file - see
	// LobContent#getPreviewText()), not TEXT - only the extracted preview text is textual.
	DOCX(Category.ARCHIVE, ".docx", null, "Word Doc"),
	XLSX(Category.ARCHIVE, ".xlsx", null, "Excel Sheet"),
	PPTX(Category.ARCHIVE, ".pptx", null, "PowerPoint"),

	BINARY(Category.BINARY, ".bin", null, "Binary");

	/**
	 * How a value of a given type is rendered.
	 */
	public enum Category {
		IMAGE, TEXT, PDF, ARCHIVE, BINARY
	}

	/** Rendering category. */
	public final Category category;

	/** File extension (including the leading dot) used on export. */
	public final String extension;

	/** RSyntaxTextArea syntax style constant, or <code>null</code>. */
	public final String syntaxStyle;

	/** Human readable name. */
	public final String displayName;

	LobContentType(Category category, String extension, String syntaxStyle, String displayName) {
		this.category = category;
		this.extension = extension;
		this.syntaxStyle = syntaxStyle;
		this.displayName = displayName;
	}

	public boolean isText() {
		return category == Category.TEXT;
	}

	public boolean isImage() {
		return category == Category.IMAGE;
	}
}

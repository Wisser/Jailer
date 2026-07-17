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

import net.sf.jailer.ui.databrowser.LobValue;
import net.sf.jailer.ui.databrowser.SQLValue;

/**
 * A materialized small text LOB (CLOB/XML) cell value that is at the same time a
 * {@link SQLValue} - so SQL literal generation / row export keeps working exactly
 * as before - and recognizable by the LOB viewer (see
 * {@link LobContentSupport#isLobCell}). It carries the already-fetched (complete)
 * small text so the viewer can show it inline even when the full value cannot be
 * re-read from the database (e.g. a query result without a usable primary key).
 *
 * <p>Replaces the plain anonymous {@code SQLValue} previously used to wrap small
 * text LOBs in {@code BrowserContentPane} and {@code SQLConsole}, which the viewer
 * could not recognize as a LOB cell.
 *
 * <p>Note: this intentionally does <em>not</em> implement {@link LobValue}. That
 * tagging interface makes {@code SQLDMLBuilder.getSQLLiteral} return {@code null};
 * a small text LOB must instead be exported through its {@link #getSQLExpression()}.
 * The viewer therefore recognizes this type explicitly rather than via a tag.
 *
 * @author Ralf Wisser
 */
public final class LobCellValue implements SQLValue {

	private final String sqlExpression;
	private final String text;

	/**
	 * @param sqlExpression the SQL literal representing the value (for export)
	 * @param text          the decoded (complete) small text; shown in the grid
	 *                      and used as the viewer content
	 */
	public LobCellValue(String sqlExpression, String text) {
		this.sqlExpression = sqlExpression;
		this.text = text;
	}

	@Override
	public String getSQLExpression() {
		return sqlExpression;
	}

	/**
	 * The carried small text content (used by {@link LobContentSupport}).
	 */
	public String getText() {
		return text;
	}

	@Override
	public String toString() {
		return text;
	}
}

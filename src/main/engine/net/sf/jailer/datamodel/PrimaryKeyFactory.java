/*
 * Copyright 2007 - 2023 Ralf Wisser.
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

package net.sf.jailer.datamodel;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.Configuration;
import net.sf.jailer.database.Session;
import net.sf.jailer.extractionmodel.ExtractionModel;
import net.sf.jailer.extractionmodel.ExtractionModel.AdditionalSubject;

/**
 * Factory for {@link PrimaryKey}s. Builds the universal primary key as a
 * super-set of all created primary key.
 *
 * @author Ralf Wisser
 */
public class PrimaryKeyFactory {

	// {@link ExecutionContext} (optional)
	private final ExecutionContext executionContext;

	/**
	 * Constructor.
	 *
	 * @param executionContext {@link ExecutionContext} (optional)
	 */
	public PrimaryKeyFactory(ExecutionContext executionContext) {
		this.executionContext = executionContext;
	}

	/**
	 * {@link #getUniversalPrimaryKey()} closes the factory, no further creation
	 * of PKs is allowed then.
	 */
	private boolean closed = false;

	// accumulate PKs
	private List<List<Column>> nullablePKs = new ArrayList<List<Column>>();
	private List<List<Column>> nonnullablePKs = new ArrayList<List<Column>>();

	/**
	 * A super-set of all columns of created primary-keys.
	 */
	private PrimaryKey universalPrimaryKey = new PrimaryKey(
			new ArrayList<Column>(), false);

	/**
	 * Constructs a new primary-key.
	 *
	 * @return a newly created primary-key
	 *
	 * @exception IllegalStateException
	 *                if factory is closed
	 */
	public PrimaryKey createPrimaryKey(List<Column> columns, String tableName) {
		if (closed) {
			throw new IllegalStateException("factory is closed");
		}
		boolean hasNullableColumn = false;
		for (Column column: columns) {
			if (column.isNullable) {
				hasNullableColumn = true;
			}
		}

		PrimaryKey primaryKey = new PrimaryKey(columns, !hasNullableColumn);

		if (executionContext != null && executionContext.getUpkDomain() != null && tableName != null) {
			if (!executionContext.getUpkDomain().contains(tableName)) {
				return primaryKey;
			}
		}

		if (hasNullableColumn) {
			nullablePKs.add(columns);
		} else {
			nonnullablePKs.add(columns);
		}
		return primaryKey;
	}

	private void createUPK() {
		for (List<Column> columns: nonnullablePKs) {
			appendUPK(columns, false);
		}
		universalPrimaryKey.numberOfIndexedPKColumns = universalPrimaryKey.getColumns().size();
		for (List<Column> columns: nullablePKs) {
			appendUPK(columns, true);
		}
	}

	private void appendUPK(List<Column> columns, boolean minimizeUPK) {
		if (Configuration.getInstance().getDoMinimizeUPK() || minimizeUPK) {
			Set<Integer> assignedUPKColumns = new HashSet<Integer>();
			for (Column column: columns) {
				boolean assigned = false;
				for (int i = 0; i < universalPrimaryKey.getColumns().size(); ++i) {
					if (assignedUPKColumns.contains(i)) {
						continue;
					}
					Column uPKColumn = universalPrimaryKey.getColumns().get(i);
					if (uPKColumn.type.equalsIgnoreCase(column.type) && column.precision < 0 && uPKColumn.precision < 0) {
						if (column.length > 0 && uPKColumn.length > 0 && column.length > uPKColumn.length) {
							// increase length
							universalPrimaryKey.getColumns().set(
									i,
									new Column(uPKColumn.name, uPKColumn.type,
											column.length, column.precision));
						}
						assigned = true;
						assignedUPKColumns.add(i);
						break;
					}
					if (uPKColumn.type.equalsIgnoreCase(column.type) && column.precision >= 0 && uPKColumn.precision >= 0
							&& (column.length >= uPKColumn.length && column.precision >= uPKColumn.precision
							 || column.length <= uPKColumn.length && column.precision <= uPKColumn.precision)) {
						if (column.length > 0 && column.length > uPKColumn.length) {
							// increase length
							universalPrimaryKey.getColumns().set(
									i,
									new Column(uPKColumn.name, uPKColumn.type,
											Math.max(column.length, uPKColumn.length),
											Math.max(column.precision,uPKColumn.precision)));
						}
						assigned = true;
						assignedUPKColumns.add(i);
						break;
					}
				}
				if (!assigned) {
					// add new columns to universal primary key
					universalPrimaryKey.getColumns().add(
							new Column(createUniqueUPKName(), column.type,
									column.length, column.precision));
					assignedUPKColumns.add(universalPrimaryKey.getColumns().size() - 1);
				}
			}
		} else {
			int n = 0;
			if (!columns.isEmpty()) {
				for (int i = 0; i < universalPrimaryKey.getColumns().size(); ++i) {
					Column uPKColumn = universalPrimaryKey.getColumns().get(i);
					Column column = columns.get(n);

					if(PrimaryKey.isAssignable(uPKColumn,column)) {
						++n;
					} else {
						if(PrimaryKey.isIncreasable(uPKColumn, column)) {
							// increase length
							universalPrimaryKey.getColumns().set(
									i,
									new Column(uPKColumn.name, uPKColumn.type,
											Math.max(column.length, uPKColumn.length),
											Math.max(column.precision,uPKColumn.precision)));
							++n;
						}

					}
					if(n>=columns.size()) {
						break;
					}
				}
			}
			// add new columns to universal primary key
			for (; n < columns.size(); ++n) {
				Column column = columns.get(n);
				universalPrimaryKey.getColumns().add(
						new Column(createUniqueUPKName(), column.type,
								column.length, column.precision));
			}
		}
	}

	/**
	 * Creates a unique name for a new universal primary key column.
	 *
	 * @return a unique name for a new universal primary key column
	 */
	private String createUniqueUPKName() {
		return "PK" + universalPrimaryKey.getColumns().size();
	}

	/**
	 * Gets the primary-key to be used for the entity-table and closes the
	 * factory.
	 *
	 * @param session
	 *            for guessing null-values of columns
	 * @return the universal primary key
	 */
	public PrimaryKey getUniversalPrimaryKey(Session session) {
		createUPK();
		closed = true;
		return universalPrimaryKey;
	}

	public static void createUPKScope(String extractionModelFile, ExecutionContext executionContext) throws MalformedURLException, IOException {
		ExtractionModel extractionModel = new ExtractionModel(new File(extractionModelFile).toURI().toURL(), executionContext.getSourceSchemaMapping(), executionContext.getParameters(), executionContext, true);
		Set<Table> subjects = new HashSet<Table>();
		if (extractionModel.additionalSubjects != null) {
			for (AdditionalSubject as: extractionModel.additionalSubjects) {
				subjects.add(as.getSubject());
			}
		}
		subjects.add(extractionModel.subject);

		Set<Table> closure = new HashSet<Table>();
		for (Table subject: subjects) {
			for (Table table: subject.closure(closure)) {
				closure.add(table);
			}
		}
		DataModel.addRestrictedDependencyWithNulledFK(closure);
		Set<String> upkDomain = new HashSet<String>();
		closure.forEach(table -> upkDomain.add(table.getName()));
		executionContext.setUpkDomain(upkDomain);
	}

}

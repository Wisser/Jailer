/*
 * Copyright 2007 the original author or authors.
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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import net.sf.jailer.ExplainTool;
import net.sf.jailer.database.StatementExecutor;
import net.sf.jailer.util.SqlUtil;

/**
 * Factory for {@link PrimaryKey}s.
 * Builds the universal primary key as a super-set of all created primary key.
 * 
 * @author Wisser
 */
public class PrimaryKeyFactory {

	/**
     * The logger.
     */
    private static final Logger _log = Logger.getLogger(ExplainTool.class);;

    /**
     * {@link #getUniversalPrimaryKey()} closes the factory, no further creation of PKs is allowed then.
     */
    private boolean closed = false; 
    
    /**
     * A super-set of all columns of created primary-keys.
     */
    private PrimaryKey universalPrimaryKey = new PrimaryKey(new ArrayList<Column>());

    /**
     * Constructs a new primary-key.
     * 
     * @return a newly created primary-key
     * 
     * @exception IllegalStateException if factory is closed
     */
    public PrimaryKey createPrimaryKey(List<Column> columns) {
        if (closed) {
            throw new IllegalStateException("factory is closed");
        }
        PrimaryKey primaryKey = new PrimaryKey(columns);
        
        int n = 0;
        if (!columns.isEmpty()) {
	        for (int i = 0; i <  universalPrimaryKey.getColumns().size(); ++i) {
	            Column uPKColumn =  universalPrimaryKey.getColumns().get(i);
	            Column column = columns.get(n);
	            if (uPKColumn.type.equals(column.type)) {
	                if (column.length > 0 && column.length > uPKColumn.length) {
	                    // increase size
	                    universalPrimaryKey.getColumns().set(i, new Column(uPKColumn.name, uPKColumn.type, column.length));
	                }
	                ++n;
	                if (n >= columns.size()) {
	                    break;
	                }
	            }
	        }
        }
        // add new columns to universal primary key
        for (; n < columns.size(); ++n) {
            Column column = columns.get(n);
            universalPrimaryKey.getColumns().add(new Column(createUniqueUPKName(), column.type, column.length));
        }
        return primaryKey;
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
     * Gets the primary-key to be used for the entity-table and closes the factory.
     *
     * @param statementExecutor for guessing null-values of columns
     * @return the universal primary key
     */
    public PrimaryKey getUniversalPrimaryKey(StatementExecutor statementExecutor) {
        closed = true;
        if (statementExecutor != null) {
        	guessNullValues(universalPrimaryKey, statementExecutor);
        }
        return universalPrimaryKey;
    }

    /**
     * Guesses null-values for each column of a primary key.
     * 
     * @param primaryKey the primary key
     */
	private void guessNullValues(PrimaryKey primaryKey, StatementExecutor statementExecutor) {
		Object[] potNulls = new Object[] {
		    new Character('0'),
			new Integer(0),
			new Date(new java.util.Date().getTime()),
		    new Byte((byte) 0),
			BigInteger.ZERO,
			new BigDecimal(0),
			new Timestamp(new java.util.Date().getTime()),
			"0",
			new Double(0.0),
		    new Float(0.0f)
		};

		_log.info("begin guessing null values");
		String drop = "DROP TABLE JL_TMP";
		for (Column column: primaryKey.getColumns()) {
			String create = "CREATE TABLE JL_TMP(c " + column.type + ")";
			try {
				statementExecutor.execute(drop);
			} catch (Exception e) {
				_log.info(e.getMessage());
			}
			try {
				statementExecutor.execute(create);
			} catch (Exception e) {
				_log.info(e.getMessage());
			}
			for (Object potNull: potNulls) {
				try {
					final StringBuffer sb = new StringBuffer();
					statementExecutor.executeUpdate("DELETE FROM JL_TMP");
					statementExecutor.executeUpdate("INSERT INTO JL_TMP(c) VALUES(?)", new Object[] { potNull });
					statementExecutor.executeQuery("SELECT c FROM JL_TMP", new StatementExecutor.ResultSetReader() {
						public void readCurrentRow(ResultSet resultSet) throws SQLException {
							sb.setLength(0);
							sb.append(SqlUtil.toSql(resultSet.getObject(1)));
						}
						public void close() {
						}
					});
					if (sb.length() > 0) {
						statementExecutor.executeUpdate("DELETE FROM JL_TMP");
						statementExecutor.executeUpdate("INSERT INTO JL_TMP(c) VALUES(" + sb.toString() + ")");
						column.nullValue = sb.toString();
						_log.info("null value for " + column + " is " + column.nullValue);
						break;
					}
				} catch (Exception e) {
					_log.info(e.getMessage());
				}
			}
		}
		try {
			statementExecutor.execute(drop);
		} catch (Exception e) {
			_log.info(e.getMessage());
		}
		_log.info("end guessing null values");
	}
    
}

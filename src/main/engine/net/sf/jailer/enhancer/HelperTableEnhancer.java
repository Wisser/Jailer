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
package net.sf.jailer.enhancer;

import java.io.IOException;
import java.io.Writer;
import java.sql.SQLException;
import java.util.Set;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.SQLDialect;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.UPSERT_MODE;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.subsetting.ScriptType;

/**
 * Inserts DDL for helper tables.
 * 
 * @author Ralf Wisser
 */
public class HelperTableEnhancer implements ScriptEnhancer {
	
	/**
	 * Is JL_DUAL needed?
	 */
	private boolean dualNeeded(Set<Table> progress, SQLDialect sqlDialect, ExecutionContext executionContext) {
		if (sqlDialect.getUpsertMode() != UPSERT_MODE.FROM_JL_DUAL) {
			return false;
		}
		if (executionContext.getUpsertOnly()) {
			return true;
		}
		for (Table table: progress) {
			if (table.getUpsert()) {
				return true;
			}
		}
		return false;
	}
	
	@Override
	public void addComments(Writer script, ScriptType scriptType,
			Session session, DBMS targetDBMSConfiguration, EntityGraph entityGraph,
			Set<Table> progress, ExecutionContext executionContext) throws IOException, SQLException {
	}

	@Override
	public void addEpilog(Writer script, ScriptType scriptType,
			Session session, DBMS targetDBMSConfiguration, EntityGraph entityGraph,
			Set<Table> progress, ExecutionContext executionContext) throws IOException, SQLException {
		if (dualNeeded(progress, targetDBMSConfiguration.getSqlDialect(), executionContext)) {
			script.append("DROP TABLE " + SQLDialect.DUAL_TABLE + ";\n");
		}
	}

	@Override
	public void addProlog(Writer script, ScriptType scriptType,
			Session session, DBMS targetDBMSConfiguration, EntityGraph entityGraph,
			Set<Table> progress, ExecutionContext executionContext) throws IOException, SQLException {
		if (dualNeeded(progress, targetDBMSConfiguration.getSqlDialect(), executionContext)) {
			script.append("CREATE TABLE " + SQLDialect.DUAL_TABLE + "(D INTEGER);\n");
			script.append("INSERT INTO " + SQLDialect.DUAL_TABLE + "(D) VALUES(1);\n");
		}
	}

}

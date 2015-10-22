/*
 * Copyright 2007 - 2015 the original author or authors.
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
package net.sf.jailer.database;

/**
 * Statements for upserts (overwrites).
 */
public enum UPSERT_MODE { 
	DB2("Select * From (values (1, 2), (3, 4)) as Q(c1, c2) Where not exists (Select * from $ T Where T.c1=Q.c1)"), 
	FROM_DUAL("Select 1, 2 From dual where not exists(Select * from $ T where T.c1=1)"),
	FROM_JL_DUAL("Select 1, 2 From $ where not exists(Select * from $ T where T.c1=1)"),
	MERGE("MERGE INTO $ T " +
                          "USING (SELECT 1 c1, 2 c2 from dual) incoming " +
                          "ON (T.c1 = incoming.c1) " +
                          "WHEN MATCHED THEN " +
                          "UPDATE SET T.c2 = incoming.c2 " +
                          "WHEN NOT MATCHED THEN " +
                          "INSERT (T.c1, T.c2) " +
                          "VALUES (incoming.c1, incoming.c2)");
	
	public final String testSQL_;
	UPSERT_MODE(String testSQL) {
		this.testSQL_ = testSQL;
	}
}
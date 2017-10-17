/*
 * Copyright 2007 - 2017 the original author or authors.
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
package net.sf.jailer.ui.databrowser.metadata;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import net.sf.jailer.util.Quoting;

/**
 * Information about a database schema.
 * 
 * @author Ralf Wisser
 */
public class MDSchema extends MDObject {

	public final boolean isDefaultSchema;
	private List<MDTable> tables;
	private static final BlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();

	/**
	 * Constructor.
	 * 
	 * @param name schema name
	 * @param isDefaultSchema <code>true</code> iff it's the users default schema
	 * @param metaDataSource the source
	 */
	public MDSchema(String name, boolean isDefaultSchema, MetaDataSource metaDataSource) {
		super(name, metaDataSource);
		this.isDefaultSchema = isDefaultSchema;
	}
	
	static {
		Thread thread = new Thread(new Runnable() {
			@Override
			public void run() {
				for (;;) {
					try {
						queue.take().run();
					} catch (Throwable t) {
					}
				}
			}
		});
        thread.setDaemon(true);
        thread.start();
	}
	
	/**
	 * Gets tables of schema
	 * 
	 * @return tables of schema
	 */
	public synchronized List<MDTable> getTables() {
		if (tables == null) {
			try {
				tables = new ArrayList<MDTable>();
	    		MetaDataSource metaDataSource = getMetaDataSource();
				synchronized (metaDataSource.getSession().getMetaData()) {
					ResultSet rs = metaDataSource.readTables(getName());
					queue.add(new Runnable() {
						@Override
						public void run() {
							try {
								Thread.sleep(1000);
							} catch (InterruptedException e) {
							}
						}
					});
					while (rs.next()) {
						String tableName = metaDataSource.getQuoting().quote(rs.getString(3));
						final MDTable table = new MDTable(tableName, this, "VIEW".equalsIgnoreCase(rs.getString(4)), "SYNONYM".equalsIgnoreCase(rs.getString(4)));
						tables.add(table);
						queue.add(new Runnable() {
							@Override
							public void run() {
								try {
									table.getColumns();
								} catch (SQLException e) {
								}
							}
						});
					}
					rs.close();
				}
				Collections.sort(tables, new Comparator<MDTable>() {
					@Override
					public int compare(MDTable o1, MDTable o2) {
						return o1.getName().compareTo(o2.getName());
					}
				});
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
		return tables;
	}

	private final Map<String, MDTable> tablePerUnquotedNameUC = new HashMap<String, MDTable>();

    /**
     * Find table by name.
     * 
     * @param tableName table name
     * @return table by name
     */
	public synchronized MDTable find(String tableName) {
		if (tablePerUnquotedNameUC.isEmpty()) {
			for (MDTable table: getTables()) {
				tablePerUnquotedNameUC.put(Quoting.staticUnquote(table.getName().toUpperCase(Locale.ENGLISH)), table);
			}
		}
		return tablePerUnquotedNameUC.get(Quoting.staticUnquote(tableName.toUpperCase(Locale.ENGLISH)));
	}

}

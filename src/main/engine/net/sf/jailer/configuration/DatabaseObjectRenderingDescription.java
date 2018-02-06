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
package net.sf.jailer.configuration;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.swing.ImageIcon;

import net.sf.jailer.database.Session;
import net.sf.jailer.modelbuilder.MetaDataCache;
import net.sf.jailer.modelbuilder.MetaDataCache.CachedResultSet;
import net.sf.jailer.ui.databrowser.metadata.MetaDataPanel;

/**
 * Describes how generic database objects are rendered (Functions, Procedures etc).
 * 
 * @author Ralf Wisser
 */
public class DatabaseObjectRenderingDescription {
	
	/**
	 * Query for a list of all objects.
	 */
	private String listQuery;
	private String iconURL;
	private String detailsIconURL;

	/**
	 * Gets query for a list of all objects.
	 * 
	 * @return query for a list of all objects
	 */
	public String getListQuery() {
		return listQuery;
	}

	/**
	 * Sets query for a list of all objects.
	 * 
	 * @param listQuery query for a list of all objects
	 */
	public void setListQuery(String listQuery) {
		this.listQuery = listQuery;
	}
	
	/**
	 * Retrieves list of all objects.
	 * 
	 * @return list of all objects 
	 */
	public CachedResultSet retrieveList(Session session, String schema) throws SQLException {
		Statement cStmt = null;
        try {
            Connection connection = session.getConnection();
            cStmt = connection.createStatement();
            ResultSet rs = cStmt.executeQuery(String.format(listQuery, schema));
            CachedResultSet result = new MetaDataCache.CachedResultSet(rs, null, session, schema);
            rs.close();
            return result;
        } catch (Exception e) {
            return null;
        } finally {
            if (cStmt != null) {
                try {
                    cStmt.close();
                } catch (SQLException e) {
                }
            }
        }
	}

	public String getIconURL() {
		return iconURL;
	}

	public void setIconURL(String iconURL) {
		this.iconURL = iconURL;
	}

	public String getDetailsIconURL() {
		return detailsIconURL;
	}

	public void setDetailsIconURL(String detailsIconURL) {
		this.detailsIconURL = detailsIconURL;
	}

	private static Map<String, ImageIcon> icons = Collections.synchronizedMap(new HashMap<String, ImageIcon>());
	
	public ImageIcon getIcon() {
		if (getIconURL() != null) {
			ImageIcon icon = icons.get(getIconURL());
			if (icon == null) {
				try {
		            icon = new ImageIcon(getClass().getResource(getIconURL()));
		        } catch (Exception e) {
		        }
			}
			icons.put(getIconURL(), icon);
			return icon;
		}
		return null;
	}

}

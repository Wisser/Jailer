package net.sf.jailer.ui.databrowser.sqlconsole;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.JComponent;

import net.sf.jailer.database.Session;
import net.sf.jailer.datamodel.Association;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;

public class MetaDataBasedSQLCompletionProvider extends SQLCompletionProvider<MetaDataSource, MDSchema, MDTable> {

	public MetaDataBasedSQLCompletionProvider(Session session, MetaDataSource metaDataSource) throws SQLException {
		super(session, metaDataSource);
	}

	@Override
	protected List<String> getColumns(MDTable table, long timeOut, JComponent waitCursorSubject) {
		try {
			return table.getColumns(timeOut, waitCursorSubject);
		} catch (SQLException e) {
			return Collections.emptyList();
		}
	}

	@Override
	protected MDSchema getDefaultSchema(MetaDataSource metaDataSource) {
		return metaDataSource.isInitialized()? metaDataSource.getDefaultSchema() : null;
	}

	@Override
	protected MDSchema findSchema(MetaDataSource metaDataSource, String name) {
		return metaDataSource.isInitialized()?  metaDataSource.find(name) : null;
	}

	@Override
	protected MDTable findTable(MDSchema schema, String name) {
		if (!schema.isLoaded()) {
			schema.loadTables(true, null, null);
    		return null;
    	}
		return schema.find(name);
	}

	@Override
	protected String getTableName(MDTable table) {
		return table.getName();
	}

	@Override
	protected List<MDTable> getTables(MDSchema schema) {
		if (!schema.isLoaded()) {
			schema.loadTables(true, null, null);
    		return Collections.emptyList();
    	}
		return schema.getTables();
	}

	@Override
	protected String getSchemaName(MDSchema schema) {
		return schema.getName();
	}

	@Override
	protected List<MDSchema> getSchemas(MetaDataSource metaDataSource) {
		return metaDataSource.getSchemas();
	}
	
	@Override
	protected List<Association> getAssociations(MDTable mdSource, MDTable mdDestination) {
		List<Association> result = new ArrayList<Association>();

		Table source = null;
		Table destination = null;
		
		if (mdSource != null) {
			source = metaDataSource.toTable(mdSource);
			if (source == null) {
				return result;
			}
		}
		if (mdDestination != null) {
			destination = metaDataSource.toTable(mdDestination);
			if (destination == null) {
				return result;
			}
		}
		
		for (Association association: source.associations) {
			if (association.destination.equals(destination) || destination == null) {
				result.add(association);
			}
		}
		return result;
	}

}
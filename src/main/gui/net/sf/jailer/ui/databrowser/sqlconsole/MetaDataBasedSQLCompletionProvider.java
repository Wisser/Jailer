package net.sf.jailer.ui.databrowser.sqlconsole;

import java.sql.SQLException;
import java.util.Collections;
import java.util.List;

import net.sf.jailer.database.Session;
import net.sf.jailer.ui.databrowser.metadata.MDSchema;
import net.sf.jailer.ui.databrowser.metadata.MDTable;
import net.sf.jailer.ui.databrowser.metadata.MetaDataSource;
import net.sf.jailer.ui.syntaxtextarea.SQLCompletionProvider;

public class MetaDataBasedSQLCompletionProvider extends SQLCompletionProvider<MetaDataSource, MDSchema, MDTable> {

	public MetaDataBasedSQLCompletionProvider(Session session, MetaDataSource metaDataSource) throws SQLException {
		super(session, metaDataSource);
	}

	@Override
	protected List<String> getColumns(MDTable table) {
		try {
			return table.getColumns();
		} catch (SQLException e) {
			return Collections.emptyList();
		}
	}

	@Override
	protected MDSchema getDefaultSchema(MetaDataSource metaDataSource) {
		return metaDataSource.getDefaultSchema();
	}

	@Override
	protected MDSchema findSchema(MetaDataSource metaDataSource, String name) {
		return metaDataSource.find(name);
	}

	@Override
	protected MDTable findTable(MDSchema schema, String name) {
		return schema.find(name);
	}

	@Override
	protected String getTableName(MDTable table) {
		return table.getName();
	}

	@Override
	protected List<MDTable> getTables(MDSchema schema) {
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

}
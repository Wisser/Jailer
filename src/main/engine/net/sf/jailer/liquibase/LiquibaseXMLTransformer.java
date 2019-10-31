package net.sf.jailer.liquibase;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.DatabaseMetaData;
import java.sql.NClob;
import java.sql.Ref;
import java.sql.ResultSet;
import java.sql.RowId;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.text.SimpleDateFormat;
import java.util.Locale;

import javax.xml.transform.sax.TransformerHandler;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

import net.sf.jailer.ExecutionContext;
import net.sf.jailer.configuration.DBMS;
import net.sf.jailer.database.Session;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;
import net.sf.jailer.subsetting.TransformerFactory;

public class LiquibaseXMLTransformer extends AbstractResultSetReader {
	
	private static final String VALUE_DATE = "valueDate";
	private static final String VALUE_NUMERIC = "valueNumeric";
	private static final String VALUE_CLOBFILE = "valueClobFile";
	private static final String VALUE_NCLOBFILE = "valueNClobFile";
	private static final String VALUE_BLOBFILE = "valueBlobFile";
	private static final String VALUE = "value";
	
	private final String rowElementName;
	private final TransformerHandler transformerHandler;
	private final File scriptFile;
	private final EntityGraph entityGraph;
	
	/**
	 * Pattern for dates.
	 */
	private final SimpleDateFormat datePattern;
	
	/**
	 * Pattern for dates.
	 */
	private final SimpleDateFormat timePattern;
	
	/**
	 * Pattern for time-stamps.
	 */
	private final SimpleDateFormat timestampPattern;
	
	/**
	 * The execution context.
	 */
	private final ExecutionContext executionContext;
	
	/**
	 * The session.
	 */
	private Session session;
	
	/**
	 * Factory.
	 */
	public static class Factory implements TransformerFactory {
		
		private final TransformerHandler transformerHandler;
		private final EntityGraph entityGraph;
		private final String scriptFile;
		private final DatabaseMetaData metaData;
		private final String datePattern;
		private final String timePattern;
		private final String timestampPattern;
		private final Session session;
		
		/**
		 * The execution context.
		 */
		private final ExecutionContext executionContext;
		
		/**
		 * Constructor.
		 * 
		 * @param table
		 *            the table to read from
		 * @param transformerHandler
		 *            to write the XML into
		 * @param metaData
		 *            database meta data
		 */
		public Factory(TransformerHandler transformerHandler, DatabaseMetaData metaData, 
				EntityGraph entityGraph, String scriptFile, String datePattern, String timePattern, String timestampPattern, Session session, ExecutionContext executionContext) {
			this.executionContext = executionContext;
			this.transformerHandler = transformerHandler;
			this.entityGraph = entityGraph;
			this.scriptFile = scriptFile;
			this.metaData = metaData;
			this.datePattern = datePattern;
			this.timePattern = timePattern;
			this.timestampPattern = timestampPattern;
			this.session = session;
		}
		
		/**
		 * Creates transformer (as {@link ResultSetReader} which 
		 * transforms rows of a given table into an external representation.
		 * 
		 * @param table the table
		 * @return a transformer
		 */
		@Override
		public ResultSetReader create(Table table) throws SQLException {
			return new LiquibaseXMLTransformer(table, transformerHandler, metaData, entityGraph, scriptFile, datePattern, timePattern, timestampPattern, session, executionContext);
		}
	}

	private LiquibaseXMLTransformer(Table table, TransformerHandler transformerHandler, DatabaseMetaData metaData, 
			EntityGraph entityGraph, String scriptFile, String datePattern, String timePattern, String timestampPattern, Session session, ExecutionContext executionContext) throws SQLException {
		this.executionContext = executionContext;
		this.transformerHandler = transformerHandler;
		this.entityGraph = entityGraph;
		this.rowElementName = qualifiedTableName(table);
		this.scriptFile = new File(scriptFile);
		this.datePattern = new SimpleDateFormat(datePattern, Locale.ENGLISH);
		this.timePattern = new SimpleDateFormat(timePattern, Locale.ENGLISH);
		this.timestampPattern = new SimpleDateFormat(timestampPattern, Locale.ENGLISH);
		this.session = session;
	}
	
	
	private String qualifiedTableName(Table t) {
		
		String schema = t.getOriginalSchema("");
		String mappedSchema = executionContext.getSchemaMapping().get(schema);
		if (mappedSchema != null) {
			schema = mappedSchema;
		}
		if (schema.length() == 0) {
			return unquote(t.getUnqualifiedName());
		}
		return unquote(schema) + "." + unquote(t.getUnqualifiedName());
	}
	
	
	private String unquote(String name) {
		
		if (!name.isEmpty()) {
			char fc = name.charAt(0);
			if (!Character.isLetterOrDigit(fc) && fc != '_') {
				String fcStr = Character.toString(fc);
				if (name.startsWith(fcStr) && name.endsWith(fcStr)) {
					name = name.substring(1, name.length() - 1);
				}
			}
		}
		return name;
	}

	
	
	@Override
	public void readCurrentRow(ResultSet singleRow) throws SQLException {

		int columnCount = getMetaData(singleRow).getColumnCount();

		try {
			AttributesImpl attrinsert = new AttributesImpl();
			attrinsert.addAttribute("", "", "tableName", "", rowElementName);

			synchronized (transformerHandler) {
				transformerHandler.startElement("", "", "insert", attrinsert);
	
				for (int i = 1; i <= columnCount; i++) {
	
					AttributesImpl attrcolumn = getColumnAttributes(singleRow, i);
	
					if(attrcolumn.getValue("name")!=null){
					transformerHandler.startElement("", "", "column", attrcolumn);
					transformerHandler.endElement("", "", "column");
					}
				}
				transformerHandler.endElement("", "", "insert");
			}
		} catch (SAXException e) {
			throw new RuntimeException(e);
		}

	}
	

	private AttributesImpl getColumnAttributes(ResultSet singleRow, int columncount) throws SQLException {		
		int count;
		AttributesImpl attrcolumn = new AttributesImpl();
		String columnname = getMetaData(singleRow).getColumnName(columncount);
		Integer columnType = getMetaData(singleRow).getColumnType(columncount);
		Integer precision = getMetaData(singleRow).getPrecision(columncount);
		
		singleRow.getObject(columncount);
	
		// special handling for sql server
		if (DBMS.MSSQL.equals(session.dbms) || DBMS.SYBASE.equals(session.dbms)) {
			if (columnType == Types.VARCHAR && precision.equals(Integer.MAX_VALUE)) {
				columnType = Types.CLOB;
			}
		}

		if (!singleRow.wasNull()){
		
			switch(columnType){

			case Types.CLOB:
				count = entityGraph.incLobCount();
				Clob clobValue = singleRow.getClob(columncount);
				int lengthc = (int) clobValue.length();
				// liquibase treats an empty clob file as though no file exists
				if (lengthc > 0) {
					String clobcontent=clobValue.getSubString(1, lengthc);
					String clobname=lobName(count, ".txt");
					attrcolumn=createAttribute(columnname,VALUE_CLOBFILE,clobname);
					writeClob(clobcontent,clobname);
				} else {
					attrcolumn=createAttribute(columnname,VALUE_CLOBFILE,null);
				}
				break;
				
			case Types.NCLOB:
				count = entityGraph.incLobCount();
				NClob nclobValue = singleRow.getNClob(columncount);
				int lengthnc = (int) nclobValue.length();
				// liquibase treats an empty clob file as though no file exists
				if (lengthnc > 0) {
					String nclobcontent=nclobValue.getSubString(1, lengthnc);
					String nclobname=lobName(count, ".txt");
					attrcolumn=createAttribute(columnname,VALUE_NCLOBFILE,nclobname);
					writeClob(nclobcontent,nclobname);
				} else {
					attrcolumn=createAttribute(columnname,VALUE_NCLOBFILE,null);
				}
				break;
				
			case Types.BLOB:
			case Types.VARBINARY:
				count = entityGraph.incLobCount();
				Blob blob = singleRow.getBlob(columncount);
				int lengthb = (int) blob.length();
				// liquibase treats an empty blob file as though no file exists
				if (lengthb > 0) {
					byte[] blobcontent = blob.getBytes(1, lengthb);
					String blobname=lobName(count, ".bin");
					attrcolumn=createAttribute(columnname,VALUE_BLOBFILE,blobname);
					writeBlob(blobcontent, blobname);
				} else {
					attrcolumn=createAttribute(columnname,VALUE_BLOBFILE,null);
				}
				break;
				
			case Types.TIMESTAMP:		
				Timestamp ts = singleRow.getTimestamp(columncount);
				String datetimestamp = timestampPattern.format(ts);
				attrcolumn=createAttribute(columnname,VALUE_DATE,datetimestamp);		
				break;
				
			case Types.TIME:	
				Timestamp t = singleRow.getTimestamp(columncount);
				String datetime = timePattern.format(t);
				attrcolumn=createAttribute(columnname,VALUE_DATE,datetime);		
				break;
				
			case Types.DATE:	
				Timestamp d = singleRow.getTimestamp(columncount);
				String datedate = datePattern.format(d);
				attrcolumn=createAttribute(columnname,VALUE_DATE,datedate);		
				break;
					
			case Types.NUMERIC:
			case Types.DECIMAL:
				BigDecimal bigdecimalvalue= singleRow.getBigDecimal(columncount); 			
				attrcolumn=createAttribute(columnname,VALUE_NUMERIC,bigdecimalvalue.toString());
				break;
				
			case Types.INTEGER:
			case Types.SMALLINT:
				Integer integervalue= singleRow.getInt(columncount);
				attrcolumn=createAttribute(columnname,VALUE_NUMERIC,integervalue.toString());
				break;
				
			case Types.FLOAT:
				Float floatvalue= singleRow.getFloat(columncount);		
				attrcolumn=createAttribute(columnname,VALUE_NUMERIC,floatvalue.toString());
				break;
				
			case Types.DOUBLE:
				Double doublevalue= singleRow.getDouble(columncount); 			
				attrcolumn=createAttribute(columnname,VALUE_NUMERIC,doublevalue.toString());		
				break;
				
			case Types.ROWID:
				RowId rowidvalue= singleRow.getRowId(columncount);
				attrcolumn=createAttribute(columnname,VALUE,rowidvalue.toString());
				break;
							
			case Types.VARCHAR:
			case Types.CHAR:
			case Types.LONGVARCHAR:
			case Types.NVARCHAR:
			case Types.NCHAR:
			case Types.LONGNVARCHAR:
				attrcolumn=createAttribute(columnname,VALUE,singleRow.getString(columncount));
				break;
				
			case Types.BOOLEAN:
				boolean booleanvalue= singleRow.getBoolean(columncount); 
				attrcolumn=createAttribute(columnname,VALUE,Boolean.toString(booleanvalue));	
				break;
				
			case Types.ARRAY:
				Array arrayvalue= singleRow.getArray(columncount); 
				attrcolumn=createAttribute(columnname,VALUE,arrayvalue.toString());				
				break;
				
			case Types.REF:
				Ref refvalue= singleRow.getRef(columncount);
				attrcolumn=createAttribute(columnname,VALUE,refvalue.toString());					
				break;
							
			default:
				attrcolumn=createAttribute(columnname,VALUE_NUMERIC,singleRow.getString(columncount));
				break;
				
//				throw new RuntimeException("Falscher Datentyp: "+singleRow.getMetaData().getColumnTypeName(columncount));
				
			}
			
		}else{
			
			attrcolumn=createAttribute(columnname,null,null);
		}
		
		return attrcolumn;
	}

	private String lobName(int count, String suffix) {
		String path = "";
		final int MAX_FILES_PER_FOLDER = 100;
		
		long i = 1;
		for (;;) {
			i *= MAX_FILES_PER_FOLDER;
			long folderNumber = (count / i) * i;
			if (folderNumber > 0) {
				path = File.separator + folderNumber + path;
			} else {
				break;
			}	
		}
		return scriptFile.getName() + ".lob" + File.separator
				+ rowElementName.toLowerCase(Locale.ENGLISH) + path + File.separator
				+ count + suffix;
	}

	private AttributesImpl createAttribute(String columnname, String valuetype,String value) {
		
		AttributesImpl attrcolumn = new AttributesImpl();
		attrcolumn.addAttribute("", "", "name", "", columnname);
		
		if((valuetype!=null) && (value!=null)){		
			attrcolumn.addAttribute("", "", valuetype, "", value);
		}
		
		return attrcolumn;
	}
	
	private void writeClob(String clobcontent, String clobname) {
		File lobFile = getLobFile(clobname);
		
		try {
			FileOutputStream fos = new FileOutputStream(lobFile);
			try {
				BufferedWriter out = new BufferedWriter(new OutputStreamWriter(fos, "UTF8"));

				try {
					out.write(clobcontent);
					out.close();

				} catch (IOException e) {
					e.printStackTrace();
				}

			} catch (UnsupportedEncodingException e1) {
				e1.printStackTrace();
			}

		} catch (FileNotFoundException e) {
			throw new RuntimeException(e);
		}
	}
	
	private void writeBlob(byte[] blobcontent, String blobname) {
		File lobFile = getLobFile(blobname);
		
		try {
			FileOutputStream fos = new FileOutputStream(lobFile);
			try {
				fos.write(blobcontent);
				fos.close();

			} catch (IOException e) {
				e.printStackTrace();
			}
		} catch (FileNotFoundException e) {
			throw new RuntimeException(e);
		}
	}

	private File getLobFile(String lobname) {
		File lobFile = new File(scriptFile.getParent(), lobname);
		File parent = lobFile.getParentFile();
		if (parent != null) {
			parent.mkdirs(); //ensure that the child table name dir exists
		}
		return lobFile;
	}
	
	@Override
	public void close() {}

}

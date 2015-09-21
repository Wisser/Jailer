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

import javax.xml.transform.sax.TransformerHandler;

import net.sf.jailer.CommandLineParser;
import net.sf.jailer.database.Session.AbstractResultSetReader;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.entitygraph.EntityGraph;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

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
	
	public LiquibaseXMLTransformer(Table table, TransformerHandler transformerHandler, DatabaseMetaData metaData, 
			EntityGraph entityGraph, String scriptFile, String datePattern, String timePattern, String timestampPattern) throws SQLException {
		this.transformerHandler = transformerHandler;
		this.entityGraph = entityGraph;
		this.rowElementName = qualifiedTableName(table);
		this.scriptFile = new File(scriptFile);
		this.datePattern = new SimpleDateFormat(datePattern);
		this.timePattern = new SimpleDateFormat(timePattern);
		this.timestampPattern = new SimpleDateFormat(timestampPattern);
	}
	
	
	private String qualifiedTableName(Table t) {
    	
    	String schema = t.getOriginalSchema("");
    	String mappedSchema = CommandLineParser.getInstance().getSchemaMapping().get(schema);
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

	
	
	public void readCurrentRow(ResultSet singleRow) throws SQLException {

		int columnCount = getMetaData(singleRow).getColumnCount();

		try {

			AttributesImpl attrinsert = new AttributesImpl();
			attrinsert.addAttribute("", "", "tableName", "", rowElementName);

			transformerHandler.startElement("", "", "insert", attrinsert);

			for (int i = 1; i <= columnCount; i++) {

				AttributesImpl attrcolumn = getColumnAttributes(singleRow, i);

				if(attrcolumn.getValue("name")!=null){
				transformerHandler.startElement("", "", "column", attrcolumn);
				transformerHandler.endElement("", "", "column");
				}
			}
			transformerHandler.endElement("", "", "insert");

		} catch (SAXException e) {
			throw new RuntimeException(e);
		}

	}
	

	private AttributesImpl getColumnAttributes(ResultSet singleRow, int columncount) throws SQLException {		
		int count;
		AttributesImpl attrcolumn = new AttributesImpl();
		String columnname = getMetaData(singleRow).getColumnName(columncount);
		Integer columnType = getMetaData(singleRow).getColumnType(columncount);
		singleRow.getObject(columncount);
				
		if (!singleRow.wasNull()){
		
			switch(columnType){

			case Types.CLOB:
				count = entityGraph.incLobCount();
				Clob clobValue = singleRow.getClob(columncount);
				int lengthc = (int) clobValue.length();	
				String clobcontent=clobValue.getSubString(1, lengthc);
				String clobname=lobName(count, ".txt");
				attrcolumn=createAttribute(columnname,VALUE_CLOBFILE,clobname);
				writeClob(clobcontent,clobname);
				break;
				
			case Types.NCLOB:
				count = entityGraph.incLobCount();
				NClob nclobValue = singleRow.getNClob(columncount);
				int lengthnc = (int) nclobValue.length();	
				String nclobcontent=nclobValue.getSubString(1, lengthnc);
				String nclobname=lobName(count, ".txt");
				attrcolumn=createAttribute(columnname,VALUE_NCLOBFILE,nclobname);
				writeClob(nclobcontent,nclobname);
				break;
				
			case Types.BLOB:
				count = entityGraph.incLobCount();
				Blob blob = singleRow.getBlob(columncount);
				byte[] blobcontent = blob.getBytes(1, (int) blob.length());
				String blobname=lobName(count, ".bin");
				attrcolumn=createAttribute(columnname,VALUE_BLOBFILE,blobname);
				writeBlob(blobcontent, blobname);
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
				+ rowElementName.toLowerCase() + path + File.separator
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
	
	public void close() {}

}

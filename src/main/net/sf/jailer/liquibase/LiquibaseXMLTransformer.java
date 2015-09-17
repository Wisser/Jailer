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
import net.sf.jailer.util.Base64;

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
	private int count=0;
	
	
	public LiquibaseXMLTransformer(Table table, TransformerHandler transformerHandler, DatabaseMetaData metaData, String scriptFile) throws SQLException {
		this.transformerHandler = transformerHandler;
		this.rowElementName = qualifiedTableName(table);
		this.scriptFile = new File(scriptFile);
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
		
		AttributesImpl attrcolumn = new AttributesImpl();
		String columnname = getMetaData(singleRow).getColumnName(columncount);
		Integer columnType = getMetaData(singleRow).getColumnType(columncount);
		singleRow.getObject(columncount);
				
		if (!singleRow.wasNull()){
		
			switch(columnType){
					
			case Types.CLOB:
				count=count+1;
				Clob clobValue = singleRow.getClob(columncount);
				int lengthc = (int) clobValue.length();	
				String clobcontent=clobValue.getSubString(1, lengthc);
				String clobname=lobName(".txt");
				attrcolumn=createAtrribute(columnname,VALUE_CLOBFILE,clobname);
				writeclob(clobcontent,clobname);
				break;
				
			case Types.NCLOB:
				count=count+1;
				NClob nclobValue = singleRow.getNClob(columncount);
				int lengthnc = (int) nclobValue.length();	
				String nclobcontent=nclobValue.getSubString(1, lengthnc);
				String nclobname=lobName(".txt");
				attrcolumn=createAtrribute(columnname,VALUE_NCLOBFILE,nclobname);
				writeclob(nclobcontent,nclobname);
				break;
				
			case Types.BLOB:
				count=count+1;			
				Blob blob = singleRow.getBlob(columncount);
				byte[] blobcontent = blob.getBytes(1, (int) blob.length());
				String blobname=lobName(".bin");
				attrcolumn=createAtrribute(columnname,VALUE_BLOBFILE,blobname);
				writeclob(Base64.encodeBytes(blobcontent),blobname);
				break;
				
			case Types.TIMESTAMP:		
				Timestamp ts = singleRow.getTimestamp(columncount);
				String datetimestamp = changeDateFormat(ts);
				attrcolumn=createAtrribute(columnname,VALUE_DATE,datetimestamp);		
				break;
				
			case Types.TIME:	
				Timestamp t = singleRow.getTimestamp(columncount);
				String datetime = changeDateFormat(t);
				attrcolumn=createAtrribute(columnname,VALUE_DATE,datetime);		
				break;
				
			case Types.DATE:	
				Timestamp d = singleRow.getTimestamp(columncount);
				String datedate = changeDateFormat(d);
				attrcolumn=createAtrribute(columnname,VALUE_DATE,datedate);		
				break;
					
			case Types.NUMERIC:
			case Types.DECIMAL:
				BigDecimal bigdecimalvalue= singleRow.getBigDecimal(columncount); 			
				attrcolumn=createAtrribute(columnname,VALUE_NUMERIC,bigdecimalvalue.toString());
				break;
				
			case Types.INTEGER:
			case Types.SMALLINT:
				Integer integervalue= singleRow.getInt(columncount);
				attrcolumn=createAtrribute(columnname,VALUE_NUMERIC,integervalue.toString());
				break;
				
			case Types.FLOAT:
				Float floatvalue= singleRow.getFloat(columncount);		
				attrcolumn=createAtrribute(columnname,VALUE_NUMERIC,floatvalue.toString());
				break;
				
			case Types.DOUBLE:
				Double doublevalue= singleRow.getDouble(columncount); 			
				attrcolumn=createAtrribute(columnname,VALUE_NUMERIC,doublevalue.toString());		
				break;
				
			case Types.ROWID:
				RowId rowidvalue= singleRow.getRowId(columncount);
				attrcolumn=createAtrribute(columnname,VALUE,rowidvalue.toString());
				break;
							
			case Types.VARCHAR:
			case Types.CHAR:
			case Types.LONGVARCHAR:
			case Types.NVARCHAR:
			case Types.NCHAR:
			case Types.LONGNVARCHAR:
				attrcolumn=createAtrribute(columnname,VALUE,singleRow.getString(columncount));
				break;
				
			case Types.BOOLEAN:
				boolean booleanvalue= singleRow.getBoolean(columncount); 
				attrcolumn=createAtrribute(columnname,VALUE,Boolean.toString(booleanvalue));	
				break;
				
			case Types.ARRAY:
				Array arrayvalue= singleRow.getArray(columncount); 
				attrcolumn=createAtrribute(columnname,VALUE,arrayvalue.toString());				
				break;
				
			case Types.REF:
				Ref refvalue= singleRow.getRef(columncount);
				attrcolumn=createAtrribute(columnname,VALUE,refvalue.toString());					
				break;
							
			default:
				attrcolumn=createAtrribute(columnname,VALUE_NUMERIC,singleRow.getString(columncount));
				break;
				
//				throw new RuntimeException("Falscher Datentyp: "+singleRow.getMetaData().getColumnTypeName(columncount));
				
			}
			
		}else{
			
			attrcolumn=createAtrribute(columnname,null,null);
		}
		
		return attrcolumn;
	}

	private String lobName(String suffix) {
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

	private AttributesImpl createAtrribute(String columnname, String valuetype,String value) {
		
		AttributesImpl attrcolumn = new AttributesImpl();
		attrcolumn.addAttribute("", "", "name", "", columnname);
		
		if((valuetype!=null) && (value!=null)){		
			attrcolumn.addAttribute("", "", valuetype, "", value);
		}
		
		return attrcolumn;
	}
	


	private void writeclob(String clobcontent, String clobname) {

		File lobFile = new File(scriptFile.getParent(), clobname);
		File parent = lobFile.getParentFile();
		if (parent != null) {
			parent.mkdirs(); //ensure that the child table name dir exists
		}
		
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


	private String changeDateFormat(Timestamp value) {
	
		String pattern = "yyyy-MM-dd'T'HH:mm:ss";
		SimpleDateFormat format = new SimpleDateFormat(pattern);			
		String dates=format.format(value);
		
		return dates;	
	}

	
	public void close() {}

}

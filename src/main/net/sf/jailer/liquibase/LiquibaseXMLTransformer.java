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

import net.sf.jailer.database.Session.ResultSetReader;
import net.sf.jailer.datamodel.Table;
import net.sf.jailer.util.Base64;

import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;

public class LiquibaseXMLTransformer implements ResultSetReader {
	
	
	
	private static final String VALUE_DATE = "valueDate";
	private static final String VALUE_NUMERIC = "valueNumeric";
	private static final String VALUE_CLOBFILE = "valueClobFile";
	private static final String VALUE_NCLOBFILE = "valueNClobFile";
	private static final String VALUE_BLOBFILE = "valueBlobFile";
	private static final String VALUE = "value";
	
	private final String rowElementName;
	private final TransformerHandler transformerHandler;
	private String ScriptFile;
	private int count=0;
	
	
	public LiquibaseXMLTransformer(Table table, TransformerHandler transformerHandler, DatabaseMetaData metaData, String sqlScriptFile) throws SQLException {
		this.transformerHandler = transformerHandler;
		this.rowElementName = qualifiedTableName(table);
		this.ScriptFile= sqlScriptFile;
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

		int columnCount = singleRow.getMetaData().getColumnCount();

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
		String columnname = singleRow.getMetaData().getColumnName(columncount);
		Integer columnType = singleRow.getMetaData().getColumnType(columncount);
		Object object =singleRow.getObject(columncount);
				
		if(object!=null){
		
			switch(columnType){
					
			case Types.CLOB:
				count=count+1;
				Clob clobValue = (Clob) object;
				int lengthc = (int) clobValue.length();	
				String clobcontent=clobValue.getSubString(1, lengthc);
				String clobname=rowElementName.toLowerCase()+"_"+count+"_clop.ch";
				attrcolumn=createAtrribute(columnname,VALUE_CLOBFILE,clobname);
				writeclob(clobcontent,clobname);
				break;
				
			case Types.NCLOB:
				count=count+1;
				NClob nclobValue = (NClob) object;
				int lengthnc = (int) nclobValue.length();	
				String nclobcontent=nclobValue.getSubString(1, lengthnc);
				String nclobname=rowElementName.toLowerCase()+"_"+count+"_clop.nch";
				attrcolumn=createAtrribute(columnname,VALUE_NCLOBFILE,nclobname);
				writeclob(nclobcontent,nclobname);
				break;
				
			case Types.BLOB:
				count=count+1;			
				Blob blob = (Blob) object;
				byte[] blobcontent = blob.getBytes(1, (int) blob.length());
				String blobname=rowElementName.toLowerCase()+"_"+count+"_clop.bin";
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
				BigDecimal bigdecimalvalue= (BigDecimal) object; 			
				attrcolumn=createAtrribute(columnname,VALUE_NUMERIC,bigdecimalvalue.toString());
				break;
				
			case Types.INTEGER:
				Integer integervalue= (Integer) object; 			
				attrcolumn=createAtrribute(columnname,VALUE_NUMERIC,integervalue.toString());
				break;
				
			case Types.FLOAT:
				Float floatvalue= (Float) object; 			
				attrcolumn=createAtrribute(columnname,VALUE_NUMERIC,floatvalue.toString());
				break;
				
			case Types.DOUBLE:
				Double doublevalue= (Double) object; 			
				attrcolumn=createAtrribute(columnname,VALUE_NUMERIC,doublevalue.toString());		
				break;
				
			case Types.ROWID:
				RowId rowidvalue= (RowId) object; 			
				attrcolumn=createAtrribute(columnname,VALUE,rowidvalue.toString());
				break;
							
			case Types.VARCHAR:
				attrcolumn=createAtrribute(columnname,VALUE,object.toString());
				break;
				
			case Types.CHAR:
				attrcolumn=createAtrribute(columnname,VALUE,object.toString());
				break;
				
			case Types.LONGVARCHAR:
				attrcolumn=createAtrribute(columnname,VALUE,object.toString());
				
				break;
				
			case Types.NVARCHAR:
				attrcolumn=createAtrribute(columnname,VALUE,object.toString());
				
				break;
				
			case Types.NCHAR:
				attrcolumn=createAtrribute(columnname,VALUE,object.toString());
				break;
				
			case Types.LONGNVARCHAR:
				attrcolumn=createAtrribute(columnname,VALUE,object.toString());
				break;
				
			case Types.BOOLEAN:
				boolean booleanvalue= singleRow.getBoolean(columncount); 
				attrcolumn=createAtrribute(columnname,VALUE,Boolean.toString(booleanvalue));	
				break;
				
			case Types.ARRAY:
				Array arrayvalue= (Array) object; 
				attrcolumn=createAtrribute(columnname,VALUE,arrayvalue.toString());				
				break;
				
			case Types.REF:
				Ref refvalue= (Ref) object; 
				attrcolumn=createAtrribute(columnname,VALUE,refvalue.toString());					
				break;
							
			default:
				
				throw new RuntimeException("Falscher Datentyp: "+singleRow.getMetaData().getColumnTypeName(columncount));
				
			}
			
		}else{
			
			attrcolumn=createAtrribute(columnname,null,null);
		}
		
		return attrcolumn;
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

		File f = new File(ScriptFile);

		try {
			FileOutputStream fos = new FileOutputStream(f.getParent() + "\\"+ clobname);
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
			e.printStackTrace();
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

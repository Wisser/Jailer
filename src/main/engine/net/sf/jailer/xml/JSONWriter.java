/*
 * Copyright 2007 - 2024 Ralf Wisser.
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
package net.sf.jailer.xml;

import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;

import net.sf.jailer.xml.XmlUtil.ObjectNotationWriter;

/**
 * A writer for JSON. 
 * 
 * @author Ralf Wisser
 */
public class JSONWriter implements ObjectNotationWriter {
	private final JsonGenerator jGen;
	
	public JSONWriter(Writer out) throws IOException {
		JsonFactory jfactory = new JsonFactory();
		this.jGen = jfactory.createGenerator(out);
		this.jGen.useDefaultPrettyPrinter();
	}

	@Override
	public void writeStartObject() throws IOException {
		this.jGen.writeStartObject();
	}

	@Override
	public void writeStartArray() throws IOException {
		this.jGen.writeStartArray();;
	}

	@Override
	public void writeEndObject() throws IOException {
		this.jGen.writeEndObject();;
	}

	@Override
	public void writeEndArray() throws IOException {
		this.jGen.writeEndArray();
	}

	@Override
	public void flush() throws IOException {
		this.jGen.flush();
	}

	@Override
	public void writeFieldName(String qName) throws IOException {
		this.jGen.writeFieldName(qName);
	}

	@Override
	public void writeArrayFieldStart(String qName) throws IOException {
		this.jGen.writeArrayFieldStart(qName);
	}

	@Override
	public void writeNull() throws IOException {
		this.jGen.writeNull();
	}

	@Override
	public void writeBinary(byte[] content) throws IOException {
		this.jGen.writeBinary(content);
	}

	@Override
	public void writeBoolean(Boolean content) throws IOException {
		this.jGen.writeBoolean(content);
	}

	@Override
	public void writeNumber(BigInteger content) throws IOException {
		this.jGen.writeNumber(content);
	}

	@Override
	public void writeNumber(BigDecimal content) throws IOException {
		this.jGen.writeNumber(content);
	}

	@Override
	public void writeNumber(Double content) throws IOException {
		this.jGen.writeNumber(content);
	}

	@Override
	public void writeNumber(Float content) throws IOException {
		this.jGen.writeNumber(content);
	}

	@Override
	public void writeNumber(Integer content) throws IOException {
		this.jGen.writeNumber(content);
	}

	@Override
	public void writeNumber(Long content) throws IOException {
		this.jGen.writeNumber(content);
	}

	@Override
	public void writeNumber(Short content) throws IOException {
		this.jGen.writeNumber(content);
	}

	@Override
	public void writeString(String string) throws IOException {
		this.jGen.writeString(string);
	}

	@Override
	public void writeComment(String comment) throws IOException {
		try {
			jGen.writeStringField("j:comment", comment + " /j:comment");
		} catch (Exception e) {
			e.printStackTrace();
			// ignore
		}
	}
	
}
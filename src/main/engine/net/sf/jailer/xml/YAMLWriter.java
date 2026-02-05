/*
 * Copyright 2007 - 2026 Ralf Wisser.
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

import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.DumperOptions.FlowStyle;
import org.yaml.snakeyaml.DumperOptions.ScalarStyle;
import org.yaml.snakeyaml.emitter.Emitter;
import org.yaml.snakeyaml.events.DocumentEndEvent;
import org.yaml.snakeyaml.events.DocumentStartEvent;
import org.yaml.snakeyaml.events.ImplicitTuple;
import org.yaml.snakeyaml.events.MappingEndEvent;
import org.yaml.snakeyaml.events.MappingStartEvent;
import org.yaml.snakeyaml.events.ScalarEvent;
import org.yaml.snakeyaml.events.SequenceEndEvent;
import org.yaml.snakeyaml.events.SequenceStartEvent;
import org.yaml.snakeyaml.events.StreamEndEvent;
import org.yaml.snakeyaml.events.StreamStartEvent;

import net.sf.jailer.util.Base64;
import net.sf.jailer.xml.XmlUtil.ObjectNotationWriter;

/**
 * A writer for JSON.
 * 
 * @author Ralf Wisser
 */
public class YAMLWriter implements ObjectNotationWriter {
	private final Emitter emitter;
	private int level = 0;

	public YAMLWriter(Writer out) throws IOException {
		DumperOptions options = new DumperOptions();
		options.setSplitLines(false); // remove the line breaks
		options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK); // remove quotes
		options.setIndent(2);
		options.setPrettyFlow(true); // remove curly brackets
		emitter = new Emitter(out, options);
		emitter.emit(new StreamStartEvent(null, null));
		emitter.emit(new DocumentStartEvent(null, null, false, null, null));
	}

	@Override
	public void writeStartObject() throws IOException {
		emitter.emit(new MappingStartEvent(null, null, true, null, null, FlowStyle.BLOCK));
		++level;
	}

	@Override
	public void writeStartArray() throws IOException {
		emitter.emit(new SequenceStartEvent(null, null, true, null, null, FlowStyle.BLOCK));
		++level;
	}

	@Override
	public void writeEndObject() throws IOException {
		emitter.emit(new MappingEndEvent(null, null));
		--level;
	}

	@Override
	public void writeEndArray() throws IOException {
		emitter.emit(new SequenceEndEvent(null, null));
		--level;
	}

	@Override
	public void flush() throws IOException {
	}

	@Override
	public void writeFieldName(String qName) throws IOException {
		emitter.emit(new ScalarEvent(null, null, new ImplicitTuple(true, true), qName, null, null, ScalarStyle.PLAIN));
	}

	@Override
	public void writeArrayFieldStart(String qName) throws IOException {
		writeFieldName(qName);
		writeStartArray();
	}

	@Override
	public void writeNull() throws IOException {
		emitter.emit(new ScalarEvent(null, "tag:yaml.org,2002:null", new ImplicitTuple(true, false), "null", null, null, ScalarStyle.PLAIN));
	}

	@Override
	public void writeBinary(byte[] content) throws IOException {
		emitter.emit(new ScalarEvent(null, "tag:yaml.org,2002:binary", new ImplicitTuple(false, false), Base64.encodeBytes(content), null, null, ScalarStyle.DOUBLE_QUOTED));
	}

	@Override
	public void writeBoolean(Boolean content) throws IOException {
		emitter.emit(new ScalarEvent(null, "tag:yaml.org,2002:null", new ImplicitTuple(true, false), content.toString(), null, null, ScalarStyle.PLAIN));
	}

	@Override
	public void writeNumber(BigInteger content) throws IOException {
		writeString(content.toString());
	}

	@Override
	public void writeNumber(BigDecimal content) throws IOException {
		writeString(content.toString());
	}

	@Override
	public void writeNumber(Double content) throws IOException {
		writeString(content.toString());
	}

	@Override
	public void writeNumber(Float content) throws IOException {
		writeString(content.toString());
	}

	@Override
	public void writeNumber(Integer content) throws IOException {
		writeString(content.toString());
	}

	@Override
	public void writeNumber(Long content) throws IOException {
		writeString(content.toString());
	}

	@Override
	public void writeNumber(Short content) throws IOException {
		writeString(content.toString());
	}

	@Override
	public void writeString(String string) throws IOException {
		if (string.indexOf('\n') >= 0) {
			emitter.emit(new ScalarEvent(null, null, new ImplicitTuple(true, true), string, null, null, ScalarStyle.DOUBLE_QUOTED));
		} else {
			emitter.emit(new ScalarEvent(null, null, new ImplicitTuple(true, true), string, null, null, ScalarStyle.PLAIN));
		}
	}

	@Override
	public void writeComment(String comment) throws IOException {
		writeFieldName("j:comment");
		emitter.emit(new ScalarEvent(null, null, new ImplicitTuple(true, true), comment + " /j:comment", null, null, ScalarStyle.DOUBLE_QUOTED));
	}

	@Override
	public void close() throws IOException {
		emitter.emit(new DocumentEndEvent(null, null, false));
		emitter.emit(new StreamEndEvent(null, null));
	}
//
//	public static void main(String[] args) throws Exception {
//
//		DumperOptions options = new DumperOptions();
//		options.setSplitLines(false); // remove the line breaks
//		options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK); // remove quotes
//		options.setIndent(2);
//		options.setPrettyFlow(true); // remove curly brackets
//		Yaml yaml = new Yaml(options);
//		
//		Map<String, Object> map = new HashedMap<>();
//		map.put("t", true);
//		byte[] value = new byte[33];
//		value[1] = -110;
//		map.put("bin", value);
//		map.put("f", false);
//		map.put("x", 12);
//		map.put("nu", null);
//		map.put("nu2", "null");
//		map.put("leer", "");
//		map.put("v", "xy");
//		map.put("v12", "\"xy");
//		map.put("v14", "'xy");
//		map.put("v16", "trailing   ");
//		map.put("v18", "  leading");
//		map.put("v3", "xy:");
//		map.put("v5", "xy:e");
//		map.put("v4", "xy-");
//		map.put("-v4", "-xy-");
//		map.put("v2", "x\ny:dp\nx:");
//		map.put("v22", "  leadingx\ny:dp\ntrailing   ");
//		map.put("d-1:", new Date());
//		map.put("y", new ArrayList() {{ add(1); add(2); }});
//		// omit some code
//		yaml.dump(map , new FileWriter("c:\\tmp\\x.yaml"));
//
//		Emitter emitter = new Emitter(new FileWriter("c:\\tmp\\x2.yaml"), options);
//		emitter.emit(new StreamStartEvent(null, null));
//		emitter.emit(new DocumentStartEvent(null, null, false, null, null));
//		emitter.emit(new CommentEvent(CommentType.BLOCK, "xy", null, null));
//		emitter.emit(new MappingStartEvent(null, null, true, null, null, FlowStyle.BLOCK));
//		emitter.emit(new ScalarEvent(null, null, new ImplicitTuple(true, true), "x", null, null, ScalarStyle.PLAIN));
//		emitter.emit(new ScalarEvent(null, null, new ImplicitTuple(true, true), "1\n2:", null, null, ScalarStyle.DOUBLE_QUOTED));
//		emitter.emit(new ScalarEvent(null, null, new ImplicitTuple(true, true), "y", null, null, ScalarStyle.PLAIN));
//		emitter.emit(new ScalarEvent(null, "tag:yaml.org,2002:null", new ImplicitTuple(true, false), "null", null, null, ScalarStyle.PLAIN));
//		emitter.emit(new ScalarEvent(null, null, new ImplicitTuple(true, true), "t", null, null, ScalarStyle.PLAIN));
//		emitter.emit(new ScalarEvent(null, "tag:yaml.org,2002:null", new ImplicitTuple(true, false), "true", null, null, ScalarStyle.PLAIN));
//		emitter.emit(new MappingEndEvent(null, null));
//		emitter.emit(new DocumentEndEvent(null, null, false));
//		emitter.emit(new StreamEndEvent(null, null));
//	}
}
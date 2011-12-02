/*
 * Copyright 2007 - 2012 the original author or authors.
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
package net.sf.jailer;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import junit.framework.Assert;
import junit.framework.TestCase;
import net.sf.jailer.datamodel.ParameterHandler;

/**
 * Tests for {@link ParameterHandler}.
 * 
 * @author Ralf Wisser
 */
public class ParameterTestCase extends TestCase {

	public void testCollection() throws Exception {
		Set<String> parameter = new TreeSet<String>();
		
		ParameterHandler.collectParameter("${a1}+${a2}", parameter);
		Assert.assertEquals(new TreeSet<String>(Arrays.asList("a1", "a2")), parameter);

		parameter.clear();
		ParameterHandler.collectParameter("${a1+${a2}", parameter);
		Assert.assertEquals(new TreeSet<String>(Arrays.asList("a2")), parameter);

		parameter.clear();
		ParameterHandler.collectParameter("${}+${a2", parameter);
		Assert.assertEquals(new TreeSet<String>(), parameter);

		parameter.clear();
		ParameterHandler.collectParameter("${a a}+${${a}", parameter);
		Assert.assertEquals(new TreeSet<String>(Arrays.asList("a")), parameter);
	}
	
	public void testAssignment() throws Exception {
		Map<String, String> v = new TreeMap<String, String>();
		
		v.put("a", "123");
		v.put("abc", "");
		v.put("1", "one");
		v.put("2", "${2}.");
		
		Assert.assertEquals("", ParameterHandler.assignParameterValues("", v));
		Assert.assertEquals(" one, $1", ParameterHandler.assignParameterValues(" ${1}, $1", v));
		Assert.assertEquals(" one, 123-", ParameterHandler.assignParameterValues(" ${1}, ${a}-${abc}", v));
		Assert.assertEquals("123123one123", ParameterHandler.assignParameterValues("${a}${a}${1}${a}", v));
		Assert.assertEquals("${2}.", ParameterHandler.assignParameterValues("${2}", v));
	}

}
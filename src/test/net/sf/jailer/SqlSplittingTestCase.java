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

import junit.framework.TestCase;
import net.sf.jailer.util.SqlUtil;

/**
 * Tests for {@link SqlUtil#resolvePseudoColumns(String, String, String)}
 * 
 * @author Ralf Wisser
 */
public class SqlSplittingTestCase extends TestCase {

	public void testSplitting() throws Exception {
		assertEquals("a,\nbc,\n \ndef\n3", SqlUtil.splitDMLStatement("a,bc, def\n3", 3));

		assertEquals("abc", SqlUtil.splitDMLStatement("abc", 4));
		assertEquals("abc", SqlUtil.splitDMLStatement("abc", 3));
		assertEquals("abc\n", SqlUtil.splitDMLStatement("abc\n", 4));
		assertEquals("abc\n", SqlUtil.splitDMLStatement("abc\n", 3));

		assertEquals("abc,\ndef,\nghik,\nkl,\nmn", SqlUtil.splitDMLStatement("abc,def,ghik,kl,mn", 3));

		assertEquals("a,\nbc\n3", SqlUtil.splitDMLStatement("a,bc\n3", 3));
		assertEquals("abc\nd,e", SqlUtil.splitDMLStatement("abc\nd,e", 4));

		assertEquals("'123,4'||\n'56'", SqlUtil.splitDMLStatement("'123,456'", 6));
		assertEquals("1,\n'123,4'||\n'56'", SqlUtil.splitDMLStatement("1,'123,456'", 6));
	}
	
}
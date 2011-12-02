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
public class PseudoColumnsTestCase extends TestCase {

	public void testPseudoColumn() throws Exception {
		assertEquals("(T1.birthday - 1)=(T2.birthday - 1)", SqlUtil.resolvePseudoColumns("A.$distance=B.$distance", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1)=(T2.birthday - 1)", SqlUtil.resolvePseudoColumns("A . $distance=B . $distance", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1)=(T2.birthday - 1)", SqlUtil.resolvePseudoColumns("A.$DISTANCE=B.$DISTANCE", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1)=(T2.birthday - 1)", SqlUtil.resolvePseudoColumns("A . $DISTANCE=B . $DISTANCE", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1)=B", SqlUtil.resolvePseudoColumns("A.$DISTANCE=B", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1)=(T2.birthday - 1)", SqlUtil.resolvePseudoColumns("a.$distance=b.$distance", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1)=(T2.birthday - 1)", SqlUtil.resolvePseudoColumns("a . $distance=b . $distance", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1)=(T2.birthday - 1)", SqlUtil.resolvePseudoColumns("a.$DISTANCE=b.$DISTANCE", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1)=(T2.birthday - 1)", SqlUtil.resolvePseudoColumns("a . $DISTANCE=b . $DISTANCE", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1)=b", SqlUtil.resolvePseudoColumns("a.$DISTANCE=b", "T1", "T2", 2, 1));

		assertEquals("(T1.birthday - 1 = 0) and (T2.birthday - 1 = 0)", SqlUtil.resolvePseudoColumns("A.$is_subject and B.$is_subject", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1 = 0) and (T2.birthday - 1 = 0)", SqlUtil.resolvePseudoColumns("A . $is_subject and B . $is_subject", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1 = 0) and (T2.birthday - 1 = 0)", SqlUtil.resolvePseudoColumns("A.$IS_SUBJECT and B.$IS_SUBJECT", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1 = 0) and (T2.birthday - 1 = 0)", SqlUtil.resolvePseudoColumns("A . $IS_SUBJECT and B . $IS_SUBJECT", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1 = 0) and B", SqlUtil.resolvePseudoColumns("A.$IS_SUBJECT and B", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1 = 0) and (T2.birthday - 1 = 0)", SqlUtil.resolvePseudoColumns("a.$is_subject and b.$is_subject", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1 = 0) and (T2.birthday - 1 = 0)", SqlUtil.resolvePseudoColumns("a . $is_subject and b . $is_subject", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1 = 0) and (T2.birthday - 1 = 0)", SqlUtil.resolvePseudoColumns("a.$IS_SUBJECT and b.$IS_SUBJECT", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1 = 0) and (T2.birthday - 1 = 0)", SqlUtil.resolvePseudoColumns("a . $IS_SUBJECT and b . $IS_SUBJECT", "T1", "T2", 2, 1));
		assertEquals("(T1.birthday - 1 = 0) and b", SqlUtil.resolvePseudoColumns("a.$IS_SUBJECT and b", "T1", "T2", 2, 1));

		assertEquals("(1 = 0)", SqlUtil.resolvePseudoColumns("b . $IS_SUBJECT", "T1", null, 2, 1));
		
		assertEquals("(T1.birthday - 1)=2", SqlUtil.resolvePseudoColumns("A.$distance=B.$distance", "T1", null, 3, 1));
		assertEquals("(T1.birthday - 1)=2", SqlUtil.resolvePseudoColumns("A . $distance=B . $distance", "T1", null, 3, 1));
		assertEquals("(T1.birthday - 1)=2", SqlUtil.resolvePseudoColumns("A.$DISTANCE=B.$DISTANCE", "T1", null, 3, 1));
		assertEquals("(T1.birthday - 1)=2", SqlUtil.resolvePseudoColumns("A . $DISTANCE=B . $DISTANCE", "T1", null, 3, 1));
		assertEquals("(T1.birthday - 1)=B", SqlUtil.resolvePseudoColumns("A.$DISTANCE=B", "T1", null, 3, 1));
		assertEquals("(T1.birthday - 1)=2", SqlUtil.resolvePseudoColumns("a.$distance=b.$distance", "T1", null, 3, 1));
		assertEquals("(T1.birthday - 1)=2", SqlUtil.resolvePseudoColumns("a . $distance=b . $distance", "T1", null, 3, 1));
		assertEquals("(T1.birthday - 1)=2", SqlUtil.resolvePseudoColumns("a.$DISTANCE=b.$DISTANCE", "T1", null, 3, 1));
		assertEquals("(T1.birthday - 1)=2", SqlUtil.resolvePseudoColumns("a . $DISTANCE=b . $DISTANCE", "T1", null, 3, 1));
		assertEquals("(T1.birthday - 1)=b", SqlUtil.resolvePseudoColumns("a.$DISTANCE=b", "T1", null, 3, 1));
	}
	
}
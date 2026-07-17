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
package net.sf.jailer.ui.databrowser.lob;

/**
 * Callback for the asynchronous, on-demand retrieval of a LOB cell's full
 * content. All methods are invoked on the AWT event dispatch thread.
 *
 * @author Ralf Wisser
 */
public interface LobRetrievalCallback {

	/**
	 * The content is available (possibly a degraded, cached fallback with a
	 * {@link LobContent#getNotice() notice}).
	 */
	void onSuccess(LobContent content);

	/**
	 * Reading the content from the database failed unexpectedly.
	 */
	void onError(Throwable t);

	/**
	 * The content cannot be shown for an expected, non-error reason (e.g. the
	 * row has no primary key and no content is cached). Should be reported as an
	 * informational message, not as an error.
	 */
	void onUnavailable(String reason);

	/**
	 * The user cancelled the retrieval.
	 */
	void onCancelled();
}

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

import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.ImageIcon;

import net.sf.jailer.ui.databrowser.LobValue;

/**
 * Placeholder for a BLOB cell whose content was too large to read eagerly - only
 * its length is known ("&lt;Blob&gt; N bytes"). Unlike the plain anonymous
 * {@link LobValue} this used to be, it is mutable: a background scan (see
 * {@code BrowserContentPane}'s data browser table renderer) can later fill in a
 * guessed {@link LobContentType} by re-reading just the leading bytes from the
 * database, without re-reading the whole value.
 *
 * @author Ralf Wisser
 */
public final class BlobLengthPlaceholder implements LobValue {

	/** The BLOB's length in bytes, or -1 if it could not be determined. */
	public final long length;

	private volatile LobContentType guessedType;

	private final AtomicBoolean typeRequested = new AtomicBoolean();

	private volatile ImageIcon thumbnailIcon;

	private final AtomicBoolean thumbnailRequested = new AtomicBoolean();

	private volatile LobContentType singleEntryType;

	private final AtomicBoolean singleEntryTypeRequested = new AtomicBoolean();

	public BlobLengthPlaceholder(long length) {
		this.length = length;
	}

	/**
	 * Marks this placeholder as having a type scan in flight. Returns
	 * <code>true</code> the first time it is called (the caller should start the
	 * scan), <code>false</code> on every subsequent call (a scan is already
	 * requested or has already completed).
	 */
	public boolean markRequested() {
		return typeRequested.compareAndSet(false, true);
	}

	/**
	 * Sets the guessed type once the background scan delivers a result.
	 */
	public void setGuessedType(LobContentType guessedType) {
		this.guessedType = guessedType;
	}

	/**
	 * The guessed type, or <code>null</code> if not yet known.
	 */
	public LobContentType getGuessedType() {
		return guessedType;
	}

	/**
	 * Marks this placeholder as having a thumbnail fetch in flight (only done once
	 * the guessed type is known to be an image). Returns <code>true</code> the
	 * first time it is called (the caller should start the fetch),
	 * <code>false</code> on every subsequent call.
	 */
	public boolean markThumbnailRequested() {
		return thumbnailRequested.compareAndSet(false, true);
	}

	/**
	 * Sets a small icon rendered from the BLOB's own image content, once decoded,
	 * so the "View" button can use it instead of the generic view icon.
	 */
	public void setThumbnailIcon(ImageIcon thumbnailIcon) {
		this.thumbnailIcon = thumbnailIcon;
	}

	/**
	 * The BLOB's own content rendered as a small icon, or <code>null</code> if not
	 * (yet) available.
	 */
	public ImageIcon getThumbnailIcon() {
		return thumbnailIcon;
	}

	/**
	 * Marks this placeholder as having a single-entry-type scan in flight (only
	 * done once the guessed type is {@link LobContentType#ZIP}). Returns
	 * <code>true</code> the first time it is called (the caller should start the
	 * scan), <code>false</code> on every subsequent call.
	 */
	public boolean markSingleEntryTypeRequested() {
		return singleEntryTypeRequested.compareAndSet(false, true);
	}

	/**
	 * Sets the guessed type of a single-file ZIP archive's one entry, once the
	 * background scan delivers a result (see {@code BrowserContentPane#retrieveZipSingleEntryType}).
	 */
	public void setSingleEntryType(LobContentType singleEntryType) {
		this.singleEntryType = singleEntryType;
	}

	/**
	 * The guessed type of a single-file ZIP archive's one entry, or <code>null</code>
	 * if not (yet) known - either the scan hasn't completed, the archive has more
	 * than one entry, or scanning it exceeded the size cap.
	 */
	public LobContentType getSingleEntryType() {
		return singleEntryType;
	}

	@Override
	public String toString() {
		return (length > 0 ? "<Blob> " + LobViewerPanel.humanSize(length, false) + " bytes" : "<Blob>");
	}

	/**
	 * Plain-text fallback for the "View" button's label while no displayable
	 * type has been guessed yet (or the guess isn't "noteworthy" - see
	 * {@code BrowserContentPane#isDisplayableLobType}). Unlike {@link #toString()},
	 * this is meant to stand on its own as a button label, so it has no angle
	 * brackets.
	 */
	public String plainText() {
		return "Blob " + LobViewerPanel.humanSize(length, false);
	}
}

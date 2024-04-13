package com.github.arvyy.islisp.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.StandardCharsets;
import java.util.Optional;

/**
 * Represents lisp stream, which can be input and/or output, potentially from/to file or in-memory buffer.
 * TODO error handling in case of nulls
 */
public class LispStream implements TruffleObject, AutoCloseable {

    static final int MAX_BUFFER_SIZE = 1024;

    private final boolean canRead;
    private final boolean canWrite;
    private final InputStream input;
    private final OutputStream output;
    private final byte[] buffer;
    private final SeekableByteChannel byteChannel;
    private int bufferLength;
    private int markedBufferPos;
    private int bufferPos;
    private boolean closed = false;


    /**
     * Create lisp stream from given input / output.
     * Special handling done is if input/output are File*putStream or ByteArray*putStream.
     * In cases concerning character conversion, UTF-8 is assumed.
     *
     * @param input input stream, can be null.
     * @param output output stream, can be null.
     * @throws IOException
     */
    public LispStream(InputStream input, OutputStream output) throws IOException {
        this.input = input;
        this.output = output;
        byteChannel = null;

        if (input == null) {
            buffer = null;
        } else {
            buffer = new byte[MAX_BUFFER_SIZE];
        }
        canWrite = output != null;
        canRead = input != null;
        bufferLength = 0;
        bufferPos = 0;
        markedBufferPos = -1;
    }

    /**
     * Create lisp from given file channel.
     * @param byteChannel file channel.
     * @param canRead is file open for reading
     * @param canWrite is file open for writing
     */
    public LispStream(SeekableByteChannel byteChannel, boolean canRead, boolean canWrite) {
        this.input = null;
        this.output = null;
        this.byteChannel = byteChannel;
        this.canRead = canRead;
        this.canWrite = canWrite;
        buffer = new byte[MAX_BUFFER_SIZE];
        bufferLength = 0;
        bufferPos = 0;
        markedBufferPos = -1;
    }

    /**
     * @return codepoint, or -1 if EOF.
     * @throws IOException
     */
// checkstyle goes crazy about magic numbers..
//CHECKSTYLE:OFF
    public int readCodepoint() throws IOException {
        int firstUnsigned = readByte();
        if (firstUnsigned == -1) {
            return -1;
        }
        byte first = (byte) firstUnsigned;
        if (((first >>> 3) & 0b11110) == 0b11110) {
            var bytes = readRemainingBytes(new byte[]{first, 0, 0, 0}, 4);
            if (bytes == null) return -1;
            return new String(bytes, StandardCharsets.UTF_8).codePointAt(0);
        }
        if (((first >>> 4) & 0b1110) == 0b1110) {
            var bytes = readRemainingBytes(new byte[]{first, 0, 0, 0}, 3);
            if (bytes == null) return -1;
            return new String(bytes, StandardCharsets.UTF_8).codePointAt(0);
        }
        if (((first >>> 5) & 0b110) == 0b110) {
            var bytes = readRemainingBytes(new byte[]{first, 0, 0, 0}, 2);
            if (bytes == null) return -1;
            return new String(bytes, StandardCharsets.UTF_8).codePointAt(0);
        }
        if (((first >>> 7)) == 0) {
            var bytes = new byte[]{first};
            return new String(bytes, StandardCharsets.UTF_8).codePointAt(0);
        }
        throw new IOException(); // TODO
    }
//CHECKSTYLE:ON

    byte[] readRemainingBytes(byte[] array, int n) throws IOException {
        for (var i = 1; i < n; i++) {
            var b = readByte();
            if (b == -1) {
                return null;
            }
            array[i] = (byte) b;
        }
        return array;
    }

    /**
     * @return next byte in inputstream, or -1 if EOF. Byte value is represented as an unsigned int.
     * @throws IOException
     */
    public int readByte() throws IOException {
        ensureBuffer();
        if (bufferLength == 0) {
            return -1;
        }
        byte b = buffer[bufferPos];
        bufferPos++;
        return Byte.toUnsignedInt(b);
    }

    /**
     * Mark current position, which can be reset with `reset`.
     * @param size size in bytes to ensure in buffer.
     * @throws IOException
     */
    public void mark(int size) throws IOException {
        if (size > bufferLength - bufferPos) {
            System.arraycopy(buffer, bufferPos, buffer, 0, bufferLength);
            bufferLength -= bufferPos;
            bufferPos = 0;
            if (input != null) {
                bufferLength += input.readNBytes(buffer, bufferLength, buffer.length - bufferLength);
            } else {
                var buf = ByteBuffer.wrap(buffer, bufferLength, buffer.length - bufferLength);
                bufferLength += Math.max(0, byteChannel.read(buf));
            }
        }
        markedBufferPos = bufferPos;
    }

    /**
     * Return to position that was active when "mark" was called.
     */
    public void reset() {
        if (markedBufferPos == -1) {
            throw new RuntimeException(); //TODO
        }
        bufferPos = markedBufferPos;
        markedBufferPos = -1;
    }

    void ensureBuffer() throws IOException {
        if (bufferPos >= bufferLength) {
            if (markedBufferPos != -1) {
                System.arraycopy(buffer, markedBufferPos, buffer, 0, bufferLength - markedBufferPos);
                bufferLength -= markedBufferPos;
                bufferPos -= markedBufferPos;
                markedBufferPos = 0;
                if (input != null) {
                    bufferLength += input.readNBytes(buffer, bufferLength, buffer.length - bufferLength);
                } else {
                    var buf = ByteBuffer.wrap(buffer, bufferLength, buffer.length - bufferLength);
                    bufferLength += Math.max(0, byteChannel.read(buf));
                }
            } else {
                bufferPos = 0;
                if (input != null) {
                    bufferLength = input.readNBytes(buffer, 0, buffer.length);
                } else {
                    var buf = ByteBuffer.wrap(buffer, 0, buffer.length);
                    bufferLength = Math.max(0, byteChannel.read(buf));
                }
            }
        }
    }

    /**
     * Write codepoint to output stream using UTF-8.
     * @param i codepoint to write.
     * @throws IOException
     */
    public void writeCodepoint(int i) throws IOException {
        var str = new String(new int[]{i}, 0, 1);
        write(str);
    }

    /**
     * Write string to output stream using UTF-8.
     * @param s string to write.
     * @throws IOException
     */
    public void write(String s) throws IOException {
        if (output != null) {
            output.write(s.getBytes(StandardCharsets.UTF_8));
        } else {
            if (bufferLength != 0) {
                byteChannel.position(byteChannel.position() - bufferLength);
                bufferLength = 0;
            }
            byteChannel.write(ByteBuffer.wrap(s.getBytes(StandardCharsets.UTF_8)));
        }
    }

    /**
     * Write raw byte to stream.
     * @param b unsigned byte presented as an integer
     * @throws IOException
     */
    public void writeByte(int b) throws IOException {
        if (output != null) {
            output.write(b);
        } else {
            if (bufferLength != 0) {
                byteChannel.position(byteChannel.position() - bufferLength);
                bufferLength = 0;
            }
            byteChannel.write(ByteBuffer.wrap(new byte[]{(byte) b}));
        }
    }

    /**
     * Close input and output streams.
     *
     * @throws IOException
     */
    @Override
    public void close() throws IOException {
        if (input != null) {
            input.close();
        }
        if (output != null) {
            output.close();
        }
        if (byteChannel != null) {
            byteChannel.close();
        }
        closed = true;
    }

    /**
     * Flush outputstream.
     *
     * @throws IOException
     */
    public void flush() throws IOException {
        if (output != null) {
            output.flush();
        }
    }

    /**
     * @return if the output stream was bytearray, returns formed string.
     */
    public Optional<String> getOutputString() {
        if (output instanceof ByteArrayOutputStream baos) {
            var result = Optional.of(baos.toString(StandardCharsets.UTF_8));
            baos.reset();
            return result;
        }
        return Optional.empty();
    }

    /**
     * @return if lispstream has output stream (doesn't check if it's closed).
     */
    public boolean hasOutput() {
        return canWrite;
    }

    /**
     * @return text until next line; or null if no more input can be read.
     * @throws IOException
     */
    public String readLine() throws IOException {
        var sb = new StringBuilder();
        while (true) {
            var c = readCodepoint();
            if (c == -1) {
                if (sb.isEmpty()) {
                    return null;
                } else {
                    break;
                }
            }
            if (c == '\n') {
                break;
            }
            sb.appendCodePoint(c);
        }
        return sb.toString();
    }

    /**
     * @return if lispstream has input stream (doesn't check if it's closed).
     */
    public boolean hasInput() {
        return canRead;
    }

    /**
     *
     * @return if stream is based on a file and permits file-related operations.
     */
    public boolean isFileBased() {
        return byteChannel != null;
    }

    /**
     * @return file stream position.
     * @throws IOException
     */
    public long getFilePosition() throws IOException {
        return byteChannel.position() - bufferLength + bufferPos;
    }

    /**
     * Set file position.
     *
     * @param position file position
     * @throws IOException
     */
    public void setFilePosition(long position) throws IOException {
        bufferPos = 0;
        bufferLength = 0;
        markedBufferPos = -1;
        byteChannel.position(position);
    }

    /**
     *
     * @return true if `close` had been called.
     */
    public boolean isClosed() {
        return closed;
    }
}

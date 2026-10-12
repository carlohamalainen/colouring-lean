# Colouring with Lean

A shared, never-ending colouring page: a 10×10 grid that everyone paints together, built on the
[Datastar Lean SDK](https://github.com/starfederation/datastar-lean).

Live demo: <https://colouring-lean.hamalainen.dev/>

# Brotli memory benchmarking

The SSE stream is brotli-compressed by the server. Each setting was measured on x86-64 Linux
with 50 open streams: the extra resident memory per stream, and the bytes on the wire for a
first frame plus one changed frame.

| quality      | window | memory per stream | bytes on wire |
|--------------|--------|-------------------|---------------|
| none (plain) |        | 880 KB            | 21,344        |
| 6            | 18     | 3,133 KB          | 471           |
| 5            | 18     | 2,191 KB          | 471           |
| 4            | 18     | 1,495 KB          | 492           |
| 4            | 16     | 1,474 KB          | 491           |
| 2            | 18     | 1,208 KB          | 550           |
| 1            | 18     | 860 KB            | 1,222         |
| 1            | 16     | 880 KB            | 1,222         |
| 0            | 18     | 1,044 KB          | 1,767         |

The memory is mostly the hasher brotli picks for the quality level, not the window. Compression
is flat from quality 2 upwards because each frame is nearly a repeat of the previous one, while
qualities 0 and 1 use a fast path that compresses noticeably worse. The app uses quality 4 with a
64 KB window.

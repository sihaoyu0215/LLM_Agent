import re


def is_var_declaration(line):
    # Determine if the line is a variable declaration or dimension definition
    keywords = [
        'CHARACTER', 'DIMENSION', 'DOUBLE PRECISION', 'INTEGER',
        'REAL', 'LOGICAL', 'COMPLEX'
    ]
    s = line.strip().upper()
    return (
            s.startswith('SUBROUTINE')
            or s.startswith('INCLUDE')
            or any(s.startswith(k) for k in keywords)
            or '::' in s  # modern Fortran declaration
            or s == ''  # blank line
    )


def is_section_divider(line):
    # Determine if the line is a section divider comment
    return re.match(r'^\s*C\s*-{3,}', line)


def is_comment(line):
    # Determine if the line is a comment
    s = line.strip().upper()
    return s.startswith('C') and not is_section_divider(line)


def chunk_umat_code(lines):
    chunks = []
    chunk = []
    chunk_idx = 1
    state = 'head'  # State machine: 'head' -> 'body'
    in_declaration = False

    for i, line in enumerate(lines):
        if state == 'head':
            # Check if still in the header/declaration section
            if (
                    is_var_declaration(line)
                    or is_comment(line)
                    or (
                    is_section_divider(line)
                    and (
                            'DECLARATION' in line.upper()
                            or 'VARIABLE' in line.upper()
                    )
            )
            ):
                in_declaration = True
                chunk.append(line)
            # Check for continuation lines
            elif in_declaration and (line.lstrip().startswith('&') or line.strip() == ''):
                chunk.append(line)
            else:
                in_declaration = False
                # Save the header chunk
                if chunk:
                    chunks.append({
                        'desc': f'Chunk {chunk_idx}: Header and variable declarations',
                        'lines': chunk.copy()
                    })
                chunk_idx += 1
                chunk = []
                state = 'body'
                chunk.append(line)
        else:
            # When encountering a section divider comment (e.g. C -----), start a new chunk
            if is_section_divider(line) and chunk:
                chunks.append({
                    'desc': f'Chunk {chunk_idx}: Logical section-{chunk_idx - 1}',
                    'lines': chunk.copy()
                })
                chunk_idx += 1
                chunk = []
            chunk.append(line)
    # Final chunk
    if chunk:
        chunks.append({
            'desc': f'Chunk {chunk_idx}: Logical section-{chunk_idx - 1}',
            'lines': chunk.copy()
        })
    return chunks


def merge_small_chunks(chunks, min_lines):
    if not chunks:
        return []
    merged = []
    i = 0
    while i < len(chunks):
        chunk = chunks[i]
        if i == 0 or "Header" in chunk['desc']:
            merged.append(chunk)
            i += 1
            continue
        # valid chunk lines
        nonempty_lines = [l for l in chunk['lines'] if l.strip() and not l.strip().upper().startswith('C')]
        if len(nonempty_lines) < min_lines and len(merged) > 0:
            merged[-1]['lines'].extend(chunk['lines'])
            merged[-1]['desc'] += ' + merged'
            i += 1
        else:
            merged.append(chunk)
            i += 1
    for i, chunk in enumerate(merged):
        if i == 0 or "Header" in chunk['desc']:
            chunk['desc'] = "Chunk 1: Header and variable declarations"
        else:
            chunk['desc'] = f"Chunk {i + 1}: Logical section-{i}"
    return merged


def write_chunks(chunks):
    for idx, chunk in enumerate(chunks, 1):
        print(f"C ===== {chunk['desc']} =====")
        for line in chunk['lines']:
            print(line, end='')


def extract_code_chunks(lines, min_chunk_lines=6):
    """
    input：fortran code
    output：chunk list
    """
    chunks = chunk_umat_code(lines)
    chunks = merge_small_chunks(chunks, min_lines=min_chunk_lines)
    return chunks


def read_code_file(filepath, encoding='utf-8'):
    """Read the contents of a code file and return as a string."""
    try:
        with open(filepath, 'r', encoding=encoding) as f:
            return f.readlines()
    except Exception as e:
        print(f"Error reading {filepath}: {e}")
        return ""


if __name__ == '__main__':
    lines = read_code_file('data/umat.for')
    chunks = chunk_umat_code(lines)
    chunks = merge_small_chunks(chunks, min_lines=3)
    write_chunks(chunks)

from pathlib import Path

# ============================================================
# 1) Détection binaire / texte (helpers bas niveau)
# ============================================================

def is_probably_binary(data: bytes) -> bool:
    """
    Heuristique simple pour distinguer fichiers texte / binaires.
    On considère comme 'binaire' si une proportion significative
    d'octets est hors de la plage ASCII imprimable.
    """
    if not data:
        return False
    non_text = 0
    for b in data:
        if b in (9, 10, 13):  # TAB, LF, CR
            continue
        if b < 32 or b > 126:
            non_text += 1
    ratio = non_text / len(data)
    return ratio > 0.3


def read_head(path: Path, n: int = 4096) -> bytes:
    """
    Lit les n premiers octets du fichier (ou moins s'il est plus petit).
    Utilisé pour les détections rapides (magic, heuristique texte).
    """
    with path.open("rb") as f:
        return f.read(n)


# ============================================================
# 2) Signatures magiques (magic numbers)
# ============================================================

def detect_magic_type(head: bytes) -> str | None:
    """
    Détection par 'magic number' sur les premiers octets du fichier.
    Retourne un type générique ou None si rien d'identifié.
    """
    if head.startswith(b"%PDF"):
        return "PDF"
    if head.startswith(b"\x89PNG"):
        return "PNG"
    if head.startswith(b"\xFF\xD8\xFF"):
        return "JPEG"
    if head.startswith(b"PK\x03\x04"):
        # Fichiers ZIP, DOCX, XLSX, etc.
        return "ZIP / DOCX / XLSX"
    if head.startswith(b"MZ"):
        return "EXE_WINDOWS"
    return None


# ============================================================
# 3) Heuristiques texte par langage (COBOL, ASM, REXX, XML, JCL)
# ============================================================

def is_cobol_fixed_format(text: str) -> bool:
    """
    Détection COBOL en format fixe :
    - On cherche les divisions principales :
      IDENTIFICATION / ENVIRONMENT / DATA / PROCEDURE
    - En colonne 8 (index 7 dans la chaîne)
    - Avec une colonne 7 (index 6) qui n'est PAS '*'
    """

    required = {
        "IDENTIFICATION DIVISION.": False,
        "ENVIRONMENT DIVISION.": False,
        "DATA DIVISION.": False,
        "PROCEDURE DIVISION": False,  # souvent 'PROCEDURE DIVISION.' ou avec USING...
    }

    lines = text.splitlines()

    for line in lines:
        # On ignore les lignes trop courtes
        if len(line) < 15:
            continue

        # On travaille en upper pour simplifier
        upper_line = line.upper()

        # Colonne 7 = index 6 (1-based -> 0-based)
        col7 = upper_line[6] if len(upper_line) > 6 else " "

        # Si commentaire en col 7 -> on ignore la ligne
        if col7 == "*":
            continue

        # Colonnes à partir de 8 (index 7)
        zone_code = upper_line[7:].lstrip()

        for key in required.keys():
            if zone_code.startswith(key):
                required[key] = True

    # Critère strict : les 4 divisions trouvées
    return all(required.values())


def is_asm_like(text: str) -> bool:
    """
    Heuristique simple pour code ASSEMBLEUR z/OS (HLASM).
    """
    upper = text.upper()
    asm_keywords = ["CSECT", "START", "USING", "DC ", "DS ", "LTORG"]
    return any(k in upper for k in asm_keywords)


def is_rexx_like(text: str) -> bool:
    """
    Heuristique simple pour code REXX.
    """
    upper = text.upper()
    rexx_markers = [
        "/* REXX */",
        "ADDRESS TSO",
        "ADDRESS ISPEXEC",
        "ADDRESS ISREDIT",
        "SAY ",
        "PARSE ",
    ]
    return any(m in upper for m in rexx_markers)


def is_xml_like(text: str) -> bool:
    """
    Heuristique simple pour XML.
    """
    stripped = text.lstrip()
    if stripped.upper().startswith("<?XML") or stripped.startswith("<"):
        if "</" in text:
            return True
    return False


def is_jcl_like(text: str) -> bool:
    """
    Détection JCL IBM :
    - Lignes commençant par '//' (éventuellement après des espaces)
    - Présence de mots-clés JCL (JOB, EXEC, DD)
    On ne l'utilisera QUE si le fichier n'est pas déjà identifié comme COBOL/ASM/REXX/XML.
    """
    lines = text.splitlines()[:80]
    jcl_hits = 0
    for line in lines:
        stripped = line.lstrip()
        if stripped.startswith("//") and any(
            kw in stripped.upper() for kw in (" JOB", " EXEC", " DD ")
        ):
            jcl_hits += 1
    return jcl_hits >= 2


def detect_text_language(text: str) -> str | None:
    """
    Détection heuristique d'un langage texte :
    - COBOL (format fixe)
    - ASSEMBLEUR
    - REXX
    - XML
    - JCL (en dernier, si ce n'est pas déjà COBOL/ASM/REXX/XML)

    IMPORTANT :
    Si c'est du COBOL ou de l'ASSEMBLEUR, on NE reclasse PAS en JCL,
    même s'il contient des blocs JCL (prologues, jobs, etc.).
    """

    upper = text.upper()

    # 1) COBOL strict (format fixe)
    if is_cobol_fixed_format(text):
        return "COBOL"

    # 1bis) COBOL "permissif" : si on trouve au moins IDENTIFICATION + PROGRAM-ID
    if "IDENTIFICATION DIVISION" in upper and "PROGRAM-ID" in upper:
        return "COBOL"

    # 2) ASSEMBLEUR
    if is_asm_like(text):
        return "ASSEMBLEUR"

    # 3) REXX
    if is_rexx_like(text):
        return "REXX"

    # 4) XML
    if is_xml_like(text):
        return "XML"

    # 5) JCL SEULEMENT si ce n'est pas COBOL/ASM/REXX/XML
    if is_jcl_like(text):
        return "JCL"

    return None

# ============================================================
# 4) Fallback par extension
# ============================================================

EXT_TYPE_MAP = {
    ".cbl": "COBOL",
    ".cob": "COBOL",
    ".cpy": "COBOL_COPY",
    ".jcl": "JCL",
    ".xml": "XML",
    ".rex": "REXX",
    ".rexx": "REXX",
    ".asm": "ASSEMBLEUR",
    ".txt": "TXT",
}


# ============================================================
# 5) Fonction principale (API publique du module)
# ============================================================

def detect_logical_type(path: Path) -> str:
    """
    Détection 'pro' de la nature d'un fichier.

    Ordre :
      1. magic number (PDF, PNG, ZIP, EXE...)
      2. binaire vs texte
      3. heuristique texte (COBOL, ASM, REXX, XML, JCL)
      4. fallback extension
      5. fallback 'TEXTE' générique
    """

    if not path.is_file():
        return "NON_FICHIER"

    try:
        head = read_head(path)
    except OSError:
        return "INACCESSIBLE"

    # 1) Magic number
    magic = detect_magic_type(head)
    if magic is not None:
        return magic

    # 2) Binaire vs texte
    if is_probably_binary(head):
        return "BINAIRE"

    # 3) Décodage texte
    try:
        text = head.decode("utf-8", errors="ignore")
    except UnicodeDecodeError:
        text = head.decode("latin-1", errors="ignore")

    # 4) Heuristique de langage
    lang = detect_text_language(text)
    if lang is not None:
        return lang

    # 5) Extension
    ext = path.suffix.lower()
    if ext in EXT_TYPE_MAP:
        return EXT_TYPE_MAP[ext]

    # 6) Fallback générique
    return "TEXTE"

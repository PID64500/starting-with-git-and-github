from pathlib import Path
from collections import defaultdict
import shutil
import re

# Regex pour PROGRAM(...) : extrait ce qui est entre parenthèses
RE_PROGRAM = re.compile(r"PROGRAM\s*\(\s*([^)]+)\s*\)", re.IGNORECASE)

# Regex pour TRANSID(...) : extrait l'argument
RE_TRANSID = re.compile(r"TRANSID\s*\(\s*([^)]+)\s*\)", re.IGNORECASE)

from detect_types import detect_logical_type

# --------------------------------------------------------------------
# CONFIGURATION GENERALE
# --------------------------------------------------------------------

# Répertoire où seront copiés et triés les fichiers analysés.
# IMPORTANT : ce répertoire NE DOIT PAS être sous 'racine' pour éviter les collisions.
WORK_ROOT = Path(r"C:\Users\Utilisateur\Documents\Workplace Git\code_source_out")

def type_to_dirname(logical_type: str) -> str:
    """
    Transforme un type logique en nom de dossier valide pour Windows.
    Exemple :
        "ZIP / DOCX / XLSX" -> "ZIP_DOCX_XLSX"
    """
    cleaned = logical_type.strip()
    cleaned = cleaned.replace("/", "_").replace("\\", "_").replace(" ", "_")
    return cleaned or "INCONNU"


def nettoyer_repertoire_travail() -> None:
    """Supprime le répertoire de travail puis le recrée vide."""
    if WORK_ROOT.exists():
        shutil.rmtree(WORK_ROOT)
    WORK_ROOT.mkdir(parents=True, exist_ok=True)


def copier_fichier_par_type(source: Path, racine: Path, logical_type: str) -> Path:
    """
    Copie un fichier source dans la zone de travail,
    dans un sous-dossier correspondant à son type logique.
    """

    # Calcul du chemin relatif : ce que l'utilisateur s'attend à retrouver
    rel_path = source.relative_to(racine)

    # Destination = WORK_ROOT / type / chemin_rel
    dest_type = type_to_dirname(logical_type)
    dest_dir = WORK_ROOT / dest_type / rel_path.parent
    dest_dir.mkdir(parents=True, exist_ok=True)

    dest_file = dest_dir / source.name
    shutil.copy2(source, dest_file)

    return dest_file


# ---------------- ANALYSES STUB (plus tard rempli) ------------------

import re

# Regex pour PROGRAM(...) : extrait ce qui est entre parenthèses
RE_PROGRAM = re.compile(r"PROGRAM\s*\(\s*([^)]+)\s*\)", re.IGNORECASE)

# Regex pour TRANSID(...) : extrait l'argument
RE_TRANSID = re.compile(r"TRANSID\s*\(\s*([^)]+)\s*\)", re.IGNORECASE)


def extract_argument(arg: str) -> str:
    """
    Nettoie l'argument extrait :
    - enlève les guillemets simples ou doubles
    - enlève espaces inutiles
    """
    arg = arg.strip()
    if arg.startswith(("'", '"')) and arg.endswith(("'", '"')):
        arg = arg[1:-1]
    return arg.strip()


def analyser_cobol(files):
    """
    Analyse des fichiers COBOL :
    - LINK PROGRAM(...)
    - XCTL PROGRAM(...)
    - START TRANSID(...)
    Affiche la valeur extraite (constante ou variable).
    """
    print(f"\n[ANALYSE COBOL] {len(files)} fichier(s)")

    for path in files:
        try:
            with path.open("r", encoding="utf-8", errors="ignore") as f:
                found_any = False

                for lineno, line in enumerate(f, start=1):
                    upper = line.upper()

                    # On ignore les lignes sans EXEC CICS
                    if "EXEC" not in upper or "CICS" not in upper:
                        continue

                    # --- LINK -------------------------------------------------
                    if "LINK" in upper and "PROGRAM" in upper:
                        m = RE_PROGRAM.search(line)
                        if m:
                            raw = m.group(1)
                            value = extract_argument(raw)
                            if not found_any:
                                print(f"\n--- {path} ---")
                                found_any = True
                            print(f"{lineno:5d}  LINK   PROGRAM = {value}")

                    # --- XCTL -------------------------------------------------
                    elif "XCTL" in upper and "PROGRAM" in upper:
                        m = RE_PROGRAM.search(line)
                        if m:
                            raw = m.group(1)
                            value = extract_argument(raw)
                            if not found_any:
                                print(f"\n--- {path} ---")
                                found_any = True
                            print(f"{lineno:5d}  XCTL   PROGRAM = {value}")

                    # --- START TRANSID ----------------------------------------
                    elif "START" in upper and "TRANSID" in upper:
                        m = RE_TRANSID.search(line)
                        if m:
                            raw = m.group(1)
                            value = extract_argument(raw)
                            if not found_any:
                                print(f"\n--- {path} ---")
                                found_any = True
                            print(f"{lineno:5d}  START  TRANSID = {value}")

        except OSError as e:
            print(f"\n[ERREUR LECTURE] {path} : {e}")




def analyser_jcl(files):
    print(f"[ANALYSE JCL] {len(files)} fichier(s)")
    # TODO : JOB, EXEC, DD, datasets


def analyser_autres(files, logical_type):
    print(f"[ANALYSE {logical_type}] {len(files)} fichier(s)")


# ------------------------- PARCOURS -------------------------------

def parcourir_arbo(racine: Path) -> None:
    """
    Parcourt récursivement la source, détecte les types,
    copie dans la zone de travail et prépare les analyses.
    """

    if not racine.exists():
        print(f"Le répertoire {racine} n'existe pas.")
        return

    print(f"Parcours de : {racine}")
    nettoyer_repertoire_travail()

    files_by_type = defaultdict(list)

    for chemin in racine.rglob("*"):
        if chemin.is_dir():
            print(f"[DIR ]  {chemin}")
            continue

        logical_type = detect_logical_type(chemin)
        print(f"[FILE] {chemin} --> {logical_type}")

        dest_file = copier_fichier_par_type(chemin, racine, logical_type)
        files_by_type[logical_type].append(dest_file)

    print("\n=== ANALYSE PAR TYPE ===")
    for t, files in files_by_type.items():
        if t == "COBOL":
            analyser_cobol(files)
        elif t == "JCL":
            analyser_jcl(files)
        else:
            analyser_autres(files, t)


if __name__ == "__main__":
    racine = Path(r"C:\Users\Utilisateur\Documents\Workplace Git\Code source")
    parcourir_arbo(racine)

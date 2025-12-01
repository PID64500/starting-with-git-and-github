"""
main_more.py

Analyse spécifique des programmes COBOL :

1. Parcourt une arborescence à partir d'un répertoire racine.
2. Sélectionne les fichiers détectés comme COBOL (via detect_types).
3. Pour chaque COBOL :
   - récupère le PROGRAM-ID
   - cherche les EXEC CICS LINK PROGRAM(...)
   - cherche les EXEC CICS XCTL PROGRAM(...)
   - cherche les EXEC CICS START TRANSID(...)
4. Construit un graphe d'appels (LINK/XCTL) entre programmes.
5. Affiche, pour chaque programme appelant :
   - les appelés directs
   - tous les programmes atteignables (direct + indirect).

Ce script ne modifie pas les sources.
"""

from pathlib import Path
from collections import defaultdict
import re

from detect_types import detect_logical_type

# ------------------------------------------------------------
#  Regex et helpers
# ------------------------------------------------------------

# PROGRAM-ID. NOM-PGM
RE_PROGRAM_ID = re.compile(
    r"PROGRAM-ID\.\s*['\"]?([A-Z0-9_-]+)['\"]?",
    re.IGNORECASE,
)

# PROGRAM(...)
RE_PROGRAM = re.compile(
    r"PROGRAM\s*\(\s*([^)]+)\s*\)",
    re.IGNORECASE,
)

# TRANSID(...)
RE_TRANSID = re.compile(
    r"TRANSID\s*\(\s*([^)]+)\s*\)",
    re.IGNORECASE,
)

def is_literal_argument(raw: str) -> bool:
    """
    Indique si l'argument de PROGRAM(...) ou TRANSID(...)
    est un littéral COBOL (entre quotes) plutôt qu'une variable.

    Exemple :
      PROGRAM('SRSRRET')  -> True
      PROGRAM(WS-NEXT-PGM) -> False
    """
    s = raw.strip()
    return s.startswith(("'", '"')) and s.endswith(("'", '"'))


def extract_argument(arg: str) -> str:
    """
    Nettoie l'argument extrait d'un PROGRAM(...) ou TRANSID(...).

    - enlève les espaces autour
    - enlève les quotes simples ou doubles si présentes
    """
    arg = arg.strip()
    if arg.startswith(("'", '"')) and arg.endswith(("'", '"')):
        arg = arg[1:-1]
    return arg.strip()


def find_program_id(lines: list[str]) -> str | None:
    """
    Recherche le PROGRAM-ID dans les lignes d'un source COBOL.

    On parcourt les lignes et on renvoie le premier nom trouvé.
    """
    for line in lines:
        m = RE_PROGRAM_ID.search(line)
        if m:
            return m.group(1).strip().upper()
    return None


# ------------------------------------------------------------
#  Analyse d'un fichier COBOL
# ------------------------------------------------------------

def analyse_cobol_file(path: Path):
    """
    Analyse un fichier COBOL.

    Retourne :
      - program_id : str | None
      - calls      : liste de tuples (caller, call_type, target_program)
      - starts     : liste de tuples (program_id, transid)

    Seuls LINK/XCTL alimentent le graphe d'appels programmes.
    START TRANSID est conservé à part pour analyse transactionnelle.
    """
    try:
        text = path.read_text(encoding="utf-8", errors="ignore")
    except OSError as e:
        print(f"[ERREUR LECTURE] {path} : {e}")
        return None, [], []

    lines = text.splitlines()

    program_id = find_program_id(lines)
    if program_id is None:
        # On signale mais on continue quand même : les appels auront caller=None.
        print(f"[AVERTISSEMENT] PROGRAM-ID introuvable dans {path}")
        caller_name = None
    else:
        caller_name = program_id

    calls: list[tuple[str, str, str]] = []
    starts: list[tuple[str, str]] = []

    for lineno, line in enumerate(lines, start=1):
        upper = line.upper()

        # On ne s'intéresse qu'aux EXEC CICS
        if "EXEC" not in upper or "CICS" not in upper:
            continue

        # LINK PROGRAM(...)
        if "LINK" in upper and "PROGRAM" in upper:
            m = RE_PROGRAM.search(line)
            if m:
                raw = m.group(1)
                target = extract_argument(raw).upper()

                # On loggue toujours pour le diagnostic
                print(f"{path} [{lineno:5d}] LINK   PROGRAM = {target} (raw={raw.strip()})")

                # Mais on n'alimente le graphe QUE si c'est un littéral
                if caller_name and target and is_literal_argument(raw):
                    calls.append((caller_name, "LINK", target))

        # XCTL PROGRAM(...)
        elif "XCTL" in upper and "PROGRAM" in upper:
            m = RE_PROGRAM.search(line)
            if m:
                raw = m.group(1)
                target = extract_argument(raw).upper()

                print(f"{path} [{lineno:5d}] XCTL   PROGRAM = {target} (raw={raw.strip()})")

                if caller_name and target and is_literal_argument(raw):
                    calls.append((caller_name, "XCTL", target))

        # START TRANSID(...)
        elif "START" in upper and "TRANSID" in upper:
            m = RE_TRANSID.search(line)
            if m:
                raw = m.group(1)
                transid = extract_argument(raw).upper()
                if caller_name and transid:
                    starts.append((caller_name, transid))
                print(f"{path} [{lineno:5d}] START  TRANSID = {transid}")

    return program_id, calls, starts


# ------------------------------------------------------------
#  Construction du graphe d'appels et fermeture transitive
# ------------------------------------------------------------

def build_call_graph(calls: list[tuple[str, str, str]]):
    """
    Construit un graphe d'appels à partir d'une liste de tuples
    (caller, call_type, target_program).

    On ne garde que les appels LINK/XCTL pour la hiérarchie programme → programme.
    """
    graph: dict[str, set[str]] = defaultdict(set)

    for caller, call_type, target in calls:
        if call_type in ("LINK", "XCTL"):
            graph[caller].add(target)

    return graph


def compute_reachable(graph: dict[str, set[str]], start: str) -> set[str]:
    """
    Calcule l'ensemble des programmes atteignables à partir de `start`
    (directement ou indirectement) via le graphe d'appels.
    """
    visited: set[str] = set()
    stack = [start]

    while stack:
        current = stack.pop()
        for callee in graph.get(current, []):
            if callee not in visited:
                visited.add(callee)
                stack.append(callee)

    return visited


# ------------------------------------------------------------
#  Parcours global
# ------------------------------------------------------------

def main():
    # Répertoire à adapter à ton environnement
    racine = Path(r"C:\Users\Utilisateur\Documents\Workplace Git\Code source")

    if not racine.exists():
        print(f"Le répertoire {racine} n'existe pas.")
        return

    print(f"=== Analyse COBOL depuis : {racine} ===")

    cobol_files: list[Path] = []

    # 1) Sélection des fichiers COBOL
    for chemin in racine.rglob("*"):
        if not chemin.is_file():
            continue
        logical_type = detect_logical_type(chemin)
        if logical_type == "COBOL":
            cobol_files.append(chemin)

    print(f"\n[INFO] {len(cobol_files)} fichier(s) COBOL détecté(s).")

    all_calls: list[tuple[str, str, str]] = []
    all_starts: list[tuple[str, str]] = []
    program_ids: set[str] = set()

    # 2) Analyse de chaque COBOL
    for cob in cobol_files:
        program_id, calls, starts = analyse_cobol_file(cob)
        if program_id:
            program_ids.add(program_id)
        all_calls.extend(calls)
        all_starts.extend(starts)

    # 3) Construction du graphe d'appels
    graph = build_call_graph(all_calls)

    print("\n=== Graphe d'appels (LINK/XCTL) ===")
    for caller in sorted(graph.keys()):
        direct = sorted(graph[caller])
        reachable = sorted(compute_reachable(graph, caller))
        print(f"\nProgramme : {caller}")
        print(f"  Appels directs : {', '.join(direct) if direct else '(aucun)'}")
        print(f"  Appels directs + indirects : {', '.join(reachable) if reachable else '(aucun)'}")

    # 4) Info sur les START TRANSID (optionnel pour l'instant)
    if all_starts:
        print("\n=== START TRANSID par programme ===")
        for prog, transid in sorted(all_starts):
            print(f"  {prog}  START TRANSID {transid}")


if __name__ == "__main__":
    main()

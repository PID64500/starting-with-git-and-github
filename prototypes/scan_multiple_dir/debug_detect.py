from pathlib import Path
from detect_types import detect_logical_type, is_cobol_fixed_format, is_jcl_like


def debug_fichier(path: Path) -> None:
    print(f"=== DEBUG : {path} ===")
    print(f"type global (detect_logical_type) : {detect_logical_type(path)}")

    try:
        text = path.read_text(errors="ignore")
    except Exception as e:
        print(f"Erreur de lecture : {e}")
        return

    print(f"is_cobol_fixed_format : {is_cobol_fixed_format(text)}")
    print(f"is_jcl_like          : {is_jcl_like(text)}")

    print("\nLignes contenant 'DIVISION' :")
    for i, line in enumerate(text.splitlines(), start=1):
        if "DIVISION" in line.upper():
            print(f"{i:4d}: {repr(line[:80])}")

    print("\nPremières lignes commençant par '//' :")
    for i, line in enumerate(text.splitlines(), start=1):
        l = line.lstrip()
        if l.startswith("//"):
            print(f"{i:4d}: {repr(line[:80])}")


if __name__ == "__main__":
    # 👉 METS ICI le chemin d'un COBOL mal détecté
    fichier = Path(r"C:\Users\Utilisateur\Documents\Workplace Git\Code source\SRSRA100")
    debug_fichier(fichier)

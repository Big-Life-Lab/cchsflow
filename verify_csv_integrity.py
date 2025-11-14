#!/usr/bin/env python3
"""
Verify CSV file integrity after restructuring or merging.

This script checks that CSV files have the expected structure and row counts.
It can be used at various stages of the merge process to ensure no data
corruption has occurred.
"""

import pandas as pd
import sys
from pathlib import Path


def verify_csv(file_path, expected_rows, expected_cols, stage="unknown"):
    """
    Verify a CSV file has expected structure.

    Args:
        file_path: Path to CSV file
        expected_rows: Expected number of data rows (excluding header)
        expected_cols: Expected number of columns
        stage: Description of which stage we're verifying (for output)

    Returns:
        bool: True if verification passed, False otherwise
    """
    print(f"Verifying: {file_path}")
    print(f"  Stage: {stage}")

    try:
        df = pd.read_csv(file_path)
        actual_rows = len(df)
        actual_cols = len(df.columns)

        print(f"  Rows: {actual_rows} (expected: {expected_rows})", end="")
        if actual_rows == expected_rows:
            print(" ✓")
        else:
            print(f" ✗ MISMATCH!")
            return False

        print(f"  Columns: {actual_cols} (expected: {expected_cols})", end="")
        if actual_cols == expected_cols:
            print(" ✓")
        else:
            print(f" ✗ MISMATCH!")
            return False

        # Check for empty header columns
        empty_headers = [col for col in df.columns if col == '' or col.strip() == '']
        if empty_headers:
            print(f"  Empty headers: {len(empty_headers)} ✗ FOUND EMPTY HEADERS!")
            return False
        else:
            print(f"  Empty headers: 0 ✓")

        # Check for duplicate column names
        if len(df.columns) != len(set(df.columns)):
            duplicates = [col for col in df.columns if list(df.columns).count(col) > 1]
            print(f"  Duplicate columns: {set(duplicates)} ✗ FOUND DUPLICATES!")
            return False
        else:
            print(f"  Duplicate columns: None ✓")

        print(f"  Overall: ✓ PASSED")
        return True

    except Exception as e:
        print(f"  ERROR: {e}")
        return False


def verify_column_names(file_path, expected_columns):
    """
    Verify that CSV has the expected column names in the expected order.

    Args:
        file_path: Path to CSV file
        expected_columns: List of expected column names in order

    Returns:
        bool: True if columns match, False otherwise
    """
    try:
        df = pd.read_csv(file_path, nrows=0)  # Read only header
        actual_columns = list(df.columns)

        if actual_columns == expected_columns:
            print(f"  Column names: ✓ Match expected structure")
            return True
        else:
            print(f"  Column names: ✗ MISMATCH")
            print(f"    Expected: {expected_columns[:5]}... ({len(expected_columns)} total)")
            print(f"    Actual:   {actual_columns[:5]}... ({len(actual_columns)} total)")

            # Find differences
            missing = set(expected_columns) - set(actual_columns)
            extra = set(actual_columns) - set(expected_columns)
            if missing:
                print(f"    Missing columns: {missing}")
            if extra:
                print(f"    Extra columns: {extra}")

            # Check if just ordering is different
            if set(actual_columns) == set(expected_columns):
                print(f"    Note: Same columns, but different order")

            return False

    except Exception as e:
        print(f"  ERROR: {e}")
        return False


def main():
    """Main verification function."""
    print("=" * 80)
    print("CSV Integrity Verification")
    print("=" * 80)
    print()

    # Parse command line arguments to determine which stage
    if len(sys.argv) > 1:
        stage = sys.argv[1]
    else:
        stage = "unknown"

    # Define expected values for each stage
    stages = {
        "dev": {
            "variables.csv": {"rows": 360, "cols": 10},
            "variable_details.csv": {"rows": 3464, "cols": 16}
        },
        "formatted": {
            "variables.csv": {"rows": 360, "cols": 10},
            "variable_details.csv": {"rows": 3464, "cols": 16}
        },
        "restructured": {
            "variables.csv": {"rows": 360, "cols": 16},
            "variable_details.csv": {"rows": 3464, "cols": 22}
        },
        "final": {
            "variables.csv": {"rows": 379, "cols": 16},
            "variable_details.csv": {"rows": 3721, "cols": 22}
        }
    }

    if stage not in stages:
        print(f"Unknown stage: {stage}")
        print(f"Available stages: {', '.join(stages.keys())}")
        print(f"Usage: python3 {sys.argv[0]} <stage>")
        print(f"Example: python3 {sys.argv[0]} restructured")
        print()
        print("Defaulting to 'final' stage verification...")
        stage = "final"

    expectations = stages[stage]

    # Define file paths
    variables_csv = Path('inst/extdata/variables.csv')
    variable_details_csv = Path('inst/extdata/variable_details.csv')

    # Check if files exist
    if not variables_csv.exists():
        print(f"ERROR: {variables_csv} not found!")
        sys.exit(1)

    if not variable_details_csv.exists():
        print(f"ERROR: {variable_details_csv} not found!")
        sys.exit(1)

    # Verify both files
    success = True

    print(f"Verification Stage: {stage.upper()}")
    print("=" * 80)
    print()

    print("1. variables.csv")
    print("-" * 80)
    vars_ok = verify_csv(
        variables_csv,
        expectations["variables.csv"]["rows"],
        expectations["variables.csv"]["cols"],
        stage
    )

    # If restructured or final, verify column structure
    if stage in ["restructured", "final"]:
        expected_vars_columns = [
            'variable', 'label', 'labelLong', 'variableType',
            'databaseStart', 'variableStart', 'subject', 'section',
            'units', 'description', 'version', 'lastUpdated',
            'reviewNotes', 'ICES.confirmation', 'Observation..MD.', 'status'
        ]
        verify_column_names(variables_csv, expected_vars_columns)

    print()

    print("2. variable_details.csv")
    print("-" * 80)
    details_ok = verify_csv(
        variable_details_csv,
        expectations["variable_details.csv"]["rows"],
        expectations["variable_details.csv"]["cols"],
        stage
    )

    # If restructured or final, verify column structure
    if stage in ["restructured", "final"]:
        expected_details_columns = [
            'variable', 'dummyVariable', 'typeEnd', 'databaseStart',
            'variableStart', 'ICES.confirmation', 'typeStart', 'recEnd',
            'numValidCat', 'catLabel', 'catLabelLong', 'units',
            'recStart', 'catStartLabel', 'variableStartShortLabel',
            'variableStartLabel', 'notes', 'version', 'lastUpdated',
            'status', 'reviewNotes', 'review'
        ]
        verify_column_names(variable_details_csv, expected_details_columns)

    print()

    # Summary
    print("=" * 80)
    if vars_ok and details_ok:
        print("✓ VERIFICATION PASSED")
        print(f"Both CSV files are valid for stage: {stage}")
        sys.exit(0)
    else:
        print("✗ VERIFICATION FAILED")
        print("One or more CSV files do not match expected structure.")
        print("Please review the errors above.")
        sys.exit(1)


if __name__ == '__main__':
    main()

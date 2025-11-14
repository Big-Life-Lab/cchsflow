#!/usr/bin/env python3
"""
Restructure CSV files to add new columns and reorder existing columns.

This script is designed to be run on the dev branch AFTER formatting
standardization has been applied. It adds the new columns introduced in
v3.0.0 and reorders columns to match the feature branch structure.

All new columns are initialized with empty strings. No existing data
is modified - only the structure changes.
"""

import csv
import pandas as pd
import sys
from pathlib import Path


def restructure_variables_csv(file_path):
    """
    Restructure variables.csv to add new columns and reorder.

    Args:
        file_path: Path to variables.csv file
    """
    print(f"Restructuring {file_path}...")

    # Read the current CSV
    df = pd.read_csv(file_path)
    original_rows = len(df)

    print(f"  Original: {original_rows} rows, {len(df.columns)} columns")

    # Define new column order (from feature branch)
    new_columns_order = [
        'variable',
        'label',
        'labelLong',
        'variableType',
        'databaseStart',
        'variableStart',
        'subject',
        'section',
        'units',
        'description',
        'version',
        'lastUpdated',
        'reviewNotes',
        'ICES.confirmation',
        'Observation..MD.',
        'status'
    ]

    # Add new columns with empty values
    for col in new_columns_order:
        if col not in df.columns:
            df[col] = ''
            print(f"  Added new column: {col}")

    # Reorder columns
    df_reordered = df[new_columns_order]

    # Verify no data loss
    if len(df_reordered) != original_rows:
        print(f"  ERROR: Row count changed! {original_rows} -> {len(df_reordered)}")
        return False

    # Write with proper formatting (QUOTE_ALL, LF line endings)
    df_reordered.to_csv(
        file_path,
        index=False,
        quoting=csv.QUOTE_ALL,
        lineterminator='\n'
    )

    print(f"  Result: {len(df_reordered)} rows, {len(df_reordered.columns)} columns")
    print(f"  ✓ Successfully restructured {file_path}")
    return True


def restructure_variable_details_csv(file_path):
    """
    Restructure variable_details.csv to add new columns and reorder.

    Args:
        file_path: Path to variable_details.csv file
    """
    print(f"Restructuring {file_path}...")

    # Read the current CSV
    df = pd.read_csv(file_path)
    original_rows = len(df)

    print(f"  Original: {original_rows} rows, {len(df.columns)} columns")

    # Define new column order (from feature branch)
    new_columns_order = [
        'variable',
        'dummyVariable',
        'typeEnd',
        'databaseStart',
        'variableStart',
        'ICES.confirmation',
        'typeStart',
        'recEnd',
        'numValidCat',
        'catLabel',
        'catLabelLong',
        'units',
        'recStart',
        'catStartLabel',
        'variableStartShortLabel',
        'variableStartLabel',
        'notes',
        'version',
        'lastUpdated',
        'status',
        'reviewNotes',
        'review'
    ]

    # Add new columns with empty values
    for col in new_columns_order:
        if col not in df.columns:
            df[col] = ''
            print(f"  Added new column: {col}")

    # Reorder columns
    df_reordered = df[new_columns_order]

    # Verify no data loss
    if len(df_reordered) != original_rows:
        print(f"  ERROR: Row count changed! {original_rows} -> {len(df_reordered)}")
        return False

    # Write with proper formatting (QUOTE_MINIMAL, CRLF line endings)
    df_reordered.to_csv(
        file_path,
        index=False,
        quoting=csv.QUOTE_MINIMAL,
        lineterminator='\r\n'
    )

    print(f"  Result: {len(df_reordered)} rows, {len(df_reordered.columns)} columns")
    print(f"  ✓ Successfully restructured {file_path}")
    return True


def main():
    """Main function to restructure both CSV files."""
    print("=" * 60)
    print("CSV Restructuring Script for v3.0.0")
    print("=" * 60)
    print()

    # Define file paths
    variables_csv = Path('inst/extdata/variables.csv')
    variable_details_csv = Path('inst/extdata/variable_details.csv')

    # Check if files exist
    if not variables_csv.exists():
        print(f"ERROR: {variables_csv} not found!")
        print("Make sure you're running this from the repository root.")
        sys.exit(1)

    if not variable_details_csv.exists():
        print(f"ERROR: {variable_details_csv} not found!")
        print("Make sure you're running this from the repository root.")
        sys.exit(1)

    # Restructure both files
    success = True

    print("Phase 1: Restructuring variables.csv")
    print("-" * 60)
    if not restructure_variables_csv(variables_csv):
        success = False
    print()

    print("Phase 2: Restructuring variable_details.csv")
    print("-" * 60)
    if not restructure_variable_details_csv(variable_details_csv):
        success = False
    print()

    # Final summary
    print("=" * 60)
    if success:
        print("✓ Restructuring completed successfully!")
        print()
        print("Next steps:")
        print("1. Review the changes with: git diff inst/extdata/")
        print("2. Verify row counts: wc -l inst/extdata/*.csv")
        print("3. Commit the changes")
        sys.exit(0)
    else:
        print("✗ Restructuring failed!")
        print("Please review the errors above and try again.")
        sys.exit(1)


if __name__ == '__main__':
    main()

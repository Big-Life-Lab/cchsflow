import csv
import argparse


def quote_empty_cells(input_csv_path, output_csv_path, file_type):
    """
    Reads a CSV file, adds double quotes around empty cells, and writes
    the modified data to a new CSV file.

    Args:
        input_csv_path (str): The path to the input CSV file.
        output_csv_path (str): The path to the output CSV file.
    """
    variables_type = 'variables'
    variable_details_type = 'variable-details'
    type_values = [variables_type, variable_details_type]
    # Use the arguments
    if not file_type in type_values:
        raise Exception('invalid file_type argument')

    file_errors = []

    try:
        with open(input_csv_path, 'r', newline='', encoding='utf-8') as infile:
            reader = csv.reader(infile)
            header = next(reader)  # Read header row
            empty_header_indices = [
                i for i, item in enumerate(header) if item == ''
            ]
            if len(empty_header_indices) != 0:
                file_errors.append("Found empty headers in the file")
            fixed_header = [
                item for index,
                item in enumerate(header) if index not in empty_header_indices
            ]
            rows = list(reader)
            with open(output_csv_path, 'w', newline='', encoding='utf-8') as outfile:
                lineterminator = '\n' if file_type == variables_type else '\r\n'
                quoting = csv.QUOTE_ALL if file_type == variables_type else csv.QUOTE_MINIMAL
                writer = csv.writer(outfile, quoting=quoting, lineterminator=lineterminator)
                writer.writerow(fixed_header)  # Write header to output file

                for row in rows:
                    modified_row = []
                    for cell_index, cell in enumerate(row):
                        if cell_index in empty_header_indices:
                            continue
                        if cell.strip() == '':  # Check if cell is empty or contains only whitespace
                            modified_row.append('')  # Add double quotes around empty string
                        else:
                            modified_row.append(cell)
                    writer.writerow(modified_row)
        print(f"Successfully processed '{input_csv_path}' and saved to '{output_csv_path}'")
    except FileNotFoundError:
        print(f"Error: Input file not found at '{input_csv_path}'")
    except Exception as e:
        print(f"An error occurred: {e}")

quote_empty_cells('inst/extdata/variables.csv', 'inst/extdata/variables.csv', 'variables')
quote_empty_cells('inst/extdata/variable_details.csv', 'inst/extdata/variable_details.csv', 'variable-details')

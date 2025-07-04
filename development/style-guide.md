# Style Guide - Survival Model Calibration Plots

This document defines writing and documentation standards for this project, designed for both human developers and AI coding assistants.

## Writing Style Standards

### Base Style
**AMA Manual of Style, 11th Edition** - Standard for medical and scientific writing

### House Style Overrides

#### 1. Spelling
**Canadian English** following CIHR Style Guide:
- Behaviour (not behavior)
- Centre (not center) 
- Analyze (not analyse)
- Anemia (not anaemia)
- Licence (noun) / license (verb)
- Counselling (double 'l')

#### 2. Headings
**Critical Rule**: Main manuscript title uses Title Case. **All subsequent headings use sentence case.**

**⚠️ AI Attention Required**: AI assistants consistently default to Title Case for headings. This must be actively corrected.

**Examples**:
- ✅ Correct: "Cox model calibration methodology"  
- ❌ Incorrect: "Cox Model Calibration Methodology"
- ✅ Correct: "Data processing steps"
- ❌ Incorrect: "Data Processing Steps"

**Rule**: Only capitalize the first word and proper nouns in headings.

#### 3. Terminology
- **Covid-19** (not COVID-19)
- **Statistics Canada** (proper noun)
- **Fine-Gray model** (proper noun - named method)
- **Austin et al.** (citation format)

#### 4. Lists
- **Short phrases**: No terminal punctuation
- **Complete sentences**: Period at end of each item
- **Parallel structure**: Maintain consistent grammatical patterns

#### 5. Citations
- **In-text**: Superscript Arabic numerals
- **Placement**: Outside periods and commas, inside colons and semicolons
- **Reference list**: Numbered (not alphabetical)

### Writing Philosophy (Inspired by The Economist)

#### Clarity and Conciseness
- Ruthlessly edit for clarity
- Prefer short, direct sentences
- One main idea per sentence
- Replace jargon with simpler alternatives

#### Word Choice
- Use "use" instead of "utilize"
- Use "about" instead of "approximately"  
- Use "major" or "real" instead of "substantive"
- Choose precise, specific terms over vague generalities

#### Active Voice
Convert passive to active voice wherever possible for directness and vigor.

## Code Documentation Standards

### R Code Style
Follow **Tidyverse Style Guide** strictly:
- snake_case for functions and variables
- Explicit package namespacing (`dplyr::filter()`)
- Comprehensive roxygen2 documentation
- Input validation with `rlang::abort()`

### Function Documentation Template
```r
#' Brief description in sentence case
#'
#' @description
#' Longer description following AMA style guidelines.
#' Use active voice and clear, direct language.
#'
#' @param data Description using sentence case
#' @param time_horizon Numeric description 
#'
#' @returns Description of return object structure
#'
#' @examples
#' # Clear, executable examples
#' result <- function_name(data, time_horizon = 5)
#'
#' @family function_family
#' @export
```

### Test Documentation
```r
test_that("function description in sentence case", {
  # Human context: Explain the test purpose in plain English
  # This test verifies that calibration functions return proper structure
  
  # Setup: Create test data with known properties
  test_data <- create_test_survival_data(n = 100)
  
  # Execute: Run the function under test
  result <- plot_calibration_cox(model, test_data, time_horizon = 5)
  
  # Verify: Check expected outcomes
  expect_s3_class(result$plot, "ggplot")
  expect_true(all(result$metrics$value >= 0))
})
```

## Markdown and Quarto Standards

### File Types
- **Static documentation**: `.md` files for methodology, standards, project context
- **Computational analysis**: `.qmd` files for tutorials, reports, executable examples
- **Configuration**: `.json` and `.yml` files for linter and build configurations

### Linter Configuration

#### markdownlint (.markdownlint.json)
```json
{
  "MD013": {
    "line_length": 120,
    "code_blocks": false,
    "tables": false,
    "headings": false
  },
  "MD024": {
    "siblings_only": true
  },
  "MD033": false,
  "MD041": false,
  "MD025": true,
  "MD026": {
    "punctuation": ".,;:!?"
  }
}
```

#### Key Rules
- **MD013**: 120 character line length (exceptions for code/tables)
- **MD024**: Allow duplicate headings in different sections
- **MD025**: Single H1 per document
- **MD033**: Allow HTML tags (needed for Quarto)

### Quarto-Specific Standards

#### YAML Frontmatter
```yaml
---
title: "Document title in sentence case"
subtitle: "Subtitle in sentence case" 
format:
  html:
    toc: true
    code-fold: true
execute:
  warning: false
  message: false
---
```

#### Code Chunk Options
```r
#| label: descriptive-name
#| fig-width: 8
#| fig-height: 6
#| message: false
```

## Project-Specific Guidelines

### Survival Analysis Terminology
- **Time horizon**: Always specify units (years for clinical interpretation)
- **Calibration metrics**: Use standard abbreviations (ICI, E50, E90)
- **Model types**: "Cox proportional hazards" or "Fine-Gray subdistribution"
- **Competing risks**: "Cumulative incidence function" (CIF)

### File Organization
- **Executable content**: tutorials/, scripts/, R/
- **Static documentation**: Root level .md files
- **Generated content**: docs/, _site/ (auto-generated, gitignored)

### Version Documentation
- **Semantic versioning**: MAJOR.MINOR.PATCH
- **Change documentation**: CHANGELOG.md following Keep a Changelog format
- **Version coordination**: Update README.md badges and project status

## AI Assistant Guidelines

### Style Enforcement
AI assistants should actively check for and correct:
1. **Title Case headings** - Convert to sentence case
2. **Passive voice** - Suggest active voice alternatives  
3. **Jargon terms** - Recommend simpler alternatives
4. **Long sentences** - Suggest breaking into shorter, clearer sentences

### Documentation Quality
- Verify all code examples are executable
- Ensure function documentation follows roxygen2 standards
- Check that statistical methodology is clearly explained
- Validate that assumptions and limitations are documented

### Consistency Checks
- Terminology usage across documents
- Code style adherence in all R files
- Markdown formatting consistency
- Citation format compliance

## Automated Enforcement

### GitHub Actions Integration
The project includes automated linting for:
- **R code**: lintr for tidyverse compliance
- **Markdown**: markdownlint for documentation standards
- **Code formatting**: styler for automatic R code formatting
- **Quarto rendering**: Validation that all .qmd files render successfully

### Local Development
```bash
# Check markdown style
markdownlint **/*.md **/*.qmd

# Format R code
Rscript -e "styler::style_dir('R')"

# Lint R code  
Rscript -e "lintr::lint_dir('R')"

# Test Quarto rendering
quarto render --execute
```

## Quality Assurance

### Manual Review Checklist
- [ ] All headings use sentence case
- [ ] Canadian English spelling throughout
- [ ] Active voice preferred over passive
- [ ] Technical terms defined on first use
- [ ] Code examples are executable
- [ ] Statistical methodology clearly explained
- [ ] Citations properly formatted

### AI Assistant Reminders
- Convert "Title Case Headings" to "sentence case headings"
- Verify Canadian spelling (behaviour, centre, analyse)
- Check for AMA style compliance in scientific writing
- Ensure survival analysis terminology is used correctly
- Validate that all code examples can actually execute
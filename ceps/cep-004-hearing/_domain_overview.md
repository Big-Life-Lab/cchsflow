# CEP-004: Hearing and vision harmonization

## Overview

This CEP formalizes the harmonization of hearing and vision variables from the CCHS Health Utility Index (HUI) module, including the Washington Group disability measures introduced in 2017-2018.

## Scope

**Domain**: Sensory health (hearing and vision)
**Target population**: Adults 50+ for dementia research
**Branch**: `hearing`
**PR**: #160

## Variables in scope

### HUI hearing items (Master only)

| Variable | Description | Cycles |
|----------|-------------|--------|
| HUI06 | Group conversation without hearing aid | 2001, 2003, 2009-2010, 2013-2014 |
| HUI07 | Group conversation with hearing aid | 2001, 2003, 2009-2010, 2013-2014 |
| HUI07A | Able to hear at all | 2001, 2003, 2005, 2007-2008, 2009-2010, 2013-2014 |
| HUI08 | Quiet room without hearing aid | 2001, 2003, 2009-2010, 2013-2014 |
| HUI09 | Quiet room with hearing aid | 2001, 2003, 2009-2010, 2011-2012, 2013-2014 |

### HUI derived hearing/vision (PUMF and Master)

| Variable | Description | Cycles |
|----------|-------------|--------|
| HUIGHER | Hearing problems (grouped) | 2001-2016 (PUMF), 2001-2014 (Master) |
| HUIGVIS | Vision trouble (grouped) | 2001-2016 (PUMF), 2001-2014 (Master) |
| HUIDHER | Hearing problems (derived) | 2001-2014 (Master only) |
| HUIDVIS | Vision trouble (derived) | 2001-2014 (Master only) |

### Washington Group measures (2017-2018)

| Variable | Description | Scale |
|----------|-------------|-------|
| WDM_005 | Difficulty seeing | 1-4 (No difficulty to Cannot do at all) |
| WDM_010 | Difficulty hearing | 1-4 (No difficulty to Cannot do at all) |

## Critical findings

### 2003 Ontario exclusion

**CRITICAL**: The 2003 PUMF HUI hearing/vision variables (HUICGHER, HUICGVIS) contain NO Ontario data. Geographic coverage is limited to:
- Atlantic provinces
- Quebec

This significantly impacts Ontario-specific analyses for dementia research.

### HUI vs Washington Group scale difference

The HUI uses a 6-point scale (1-6) while Washington Group uses a 4-point scale (1-4). Harmonization requires documented crosswalk.

## Integration testing requirements

### Age cutoffs

For dementia research, test availability at:
- Age 50+
- Age 55+
- Age 60+

### Two-tier analysis

1. **Canada-wide**: Full sample sizes by cycle
2. **Ontario-specific**: Province filtered (GEO*PRV == 35)

## Workflow status

See [_workflow_state.yaml](_workflow_state.yaml) for current progress.

## Related documentation

- PR #160: Original hearing harmonization
- [cchs_available_variables_list.csv](../../development/cchs_available_variables_list.csv): Variable availability reference
- [ontario_sensory_oral_analysis.R](../../development/ontario_sensory_oral_analysis.R): Analysis script

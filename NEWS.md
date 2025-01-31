# fireexposuR (development version)

* Continuous improvement of documentation and resources

# fireexposuR 1.1.0 (January 31, 2025)

## Major updates 

* added parameter customization options to `fire_exp_dir()`:
    * customize transect lengths with `t_lengths` parameter
    * customize number of transects with `interval` parameter
    * customize high exposure threshold with `thresh_exp` parameter
    * customize viable pathway threshold with `thresh_viable` parameter
* added option to use custom classification breaks to `fire_exp_summary()`, 
`fire_exp_map_class()`, `fire_exp_extract_vis()`, `fire_exp_validate()`
    * all functions that use classes now automatically add a 'Nil' class for 
    values that are exactly 0
* split `fire_exp_dir_vis()` into two separate functions: `fire_exp_dir_plot()`,
`fire_exp_dir_map()`
* removed plotting option from `fire_exp_validate()` 
* added new function `fire_exp_validate_plot()` to visualize results from
`fire_exp_validate()`
* added options to add custom titles to functions that return plots or maps
* significant updates and additions to the documentation

# fireexposuR 1.0.1 (October 2024)

* fixes for issues that came up during initial tests using different data
* fixes for many other things that came up as I learned about R package development

# fireexposuR 1.0 (September 2024)

* the initial release to make the WIP code publicly available
* submission to ROpenSci

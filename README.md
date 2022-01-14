# descripteur

The goal of descripteur is to make basic data description easy.

## Features

 + describe data with arbitrary set of describing functions
 + compare arbitrary (possibly overlapping) subsets in different ways with
   arbitrary set of comparing functions
 + keeping 'metadata' that can be utilized for pretty tables (LaTeX)

## Data description approch

### Describing and comparing

In this package, a description of a data frame (or similar object)
consist of a grouping and 2 sets of functions, *describers* and
*comparers*. The simplest grouping is the trivial grouping where we
consider 1 group consisting of all the units, in which case there is
nothing to compare and a description is the set of describing
functions applied, in turn, to all relevant variables.

If a grouping exists, we can apply comparing functions to the
different groups. There are 2 different types of comparisons
(*comp*) available:

  +  "overall" is when the grouping is fed to some function that makes
an overall comparison, like an anova analysis (for arbitrary many
groups) or a t-test (in which case there must be exactly 2 groups).
  +  "across" and "adjacent" are pairwise comparisons of each group
  with respect to either the first, or the previous, group as given by
  order of the grouping.

If a grouping exist, this will impact the description (*desc*) (unless
specified elsewise):

  +  "each" is the describing functions applied to all groups, in
    turn. This option will be default when considering an "overall"
    comparison.
  +  "first" is the describing functions applied only to the first
    group. This will be default when considerig an "across" or
    "adjacent" comparison.


### Data types and a guide

We do not describe all kinds of data in the same manner. Thus, this
package differentiates between different kinds of data types:

 + `real` real, i.e. numeric
 + `bnry` binary, i.e. variables with 2 distinct values
 + `catg` categorical,
 + `date` date (duh!), and
 + `surv` class Surv from the survival package.

This will be determined by a 'guide', which is a data frame that for each
variables specifies how it is to be described. Any variable with only 2 distinct
values will be regarded as `bnry`, other text- and factor variables are `catg`,
the classes 'Date' and 'POSIXct' are `date` and numerical variables with enough
distinct values (more than `real.tol`) are `real`.

If you do not want to accept the defaults you can create a guide via
`dtable_guide` with non-default arguments, or manipulate the resulting
object directly.

### More details
More details can be found in the vignette `describe-data` which functions as the
major test for this package.

package ai.acyclic.prover.commons

object __OperatorPrecedence {

  /*
  operator precedence:

    LOWEST!
    (all letters)
    |
    ^
    &
    = !
    < >
    :
    + -
   * / %
    (all other special characters)
    HIGHEST!

    CAUTION: this rule doesn't apply to infix HKTs (higher kind types), e.g.
   */

  

  /*
  right association rule:

  operator with a trailing colon (e.g. `:+`) is right-associative, it is applied as if this and first arg are swapped, e.g.:

  a *: b  is equivalent to b.*:(a)

  CAUTION: this rule affects infix HKTs differently, it will change precedence but not swap positions of 2 args, e.g.
   */

  
}

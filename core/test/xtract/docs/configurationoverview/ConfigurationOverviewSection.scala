package xtract.docs.configurationoverview

import xtract.FunSuite

// doc begin
// # Configuration
// Reading can be customized in a variety of ways.
// include Params
// Read function accepts configuration via an implicit parameter.
// What you saw in intro was achieved with default parameters
import xtract.DefaultReadParams
// You can always start with defaults
// include DefaultParams
// and customize what you need via methods
// include ParamsMethods
// doc end

class ConfigurationOverviewSection extends FunSuite {
  DefaultReadParams
}

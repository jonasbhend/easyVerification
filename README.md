veriWrapper
===========

This package provides functions to simplify application of forecast verification metrcis to large datasets. The forecast metrics are imported from the Specs-Verification package. Additional verification metrics not available through Specs-Verification are implemented directly.

The design goal of the veriWrap function is to:
- Flexibility: a variety of data structures are supported
- Ease of use: Absolute forecasts and observations are converted to category and probability forecasts based on the threshold or probability (e.g. terciles) provided, ouputs are reformatted to fit the input
- Convenience and flexibility over speed: R's built-in vectorisation is used where possible but more importantly, new metrics should be easy to implement

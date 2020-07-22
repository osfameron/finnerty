'use strict';
const diff_match_patch = require("diff-match-patch");
const dmp = new diff_match_patch()
dmp.Diff_EditCost = 4 // or 5 is good

const diff = a => b => {
    var d = dmp.diff_main(a, b)
    dmp.diff_cleanupEfficiency(d)
    // dmp.diff_cleanupSemantic(d)
    return d
}

exports.diffImpl = diff

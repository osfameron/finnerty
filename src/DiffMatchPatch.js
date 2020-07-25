'use strict';
const dmp = require("diff-match-patch");

exports.diffMatchPatch = rec => {
    var dmpObj = new dmp()
    dmpObj.Diff_EditCost = rec.editCost
    return dmpObj
}

exports.diffImpl = dmpObj => a => b => {
    var d = dmpObj.diff_main(a, b)
    dmpObj.diff_cleanupEfficiency(d)
    // dmpObj.diff_cleanupSemantic(d)
    return d
}

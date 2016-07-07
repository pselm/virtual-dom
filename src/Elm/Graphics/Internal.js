/* global exports */
"use strict";

// module Elm.Graphics.Internal

exports.getDimensions = function (node) {
    return function () {
        // Get the "window" from the node, rather than assuming a global
        // window object.
        var style = node.ownerDocument.defaultView.getComputedStyle(node, null);
        // parseFloat will ignore the 'px' at the end, which is convenient ...
        var w = parseFloat(style.getPropertyValue('width'));
        var h = parseFloat(style.getPropertyValue('height'));

        return {
            width: w,
            height: h
        };
    }
};

exports.setStyle = function (key) {
    return function (value) {
        return function (element) {
            return function () {
                element.style[key] = value;
                return {};
            }
        }
    }
};

exports.removeStyle = function (key) {
    return function (element) {
        return function () {
            element.style[key] = "";
            return {};
        }
    }
};

exports.setProperty = function (key) {
    return function (value) {
        return function (element) {
            return function () {
                element[key] = value;
                return {};
            };
        };
    };
};

exports.removeProperty = function (key) {
    return function (element) {
        return function () {
            // This is really a special-case for VirtualDOM -- it's
            // what the original Javascript does. One might prefer
            // `delete element[key]`, but perhaps that causes trouble
            // in some cases.
            element[key] = (typeof element[key] === 'string') ? '' : null;
            return {};
        };
    };
};

exports.setPropertyIfDifferent = function (key) {
    return function (value) {
        return function (element) {
            return function () {
                if (element[key] !== value) {
                    element[key] = value;
                }

                return {};
            };
        };
    };
};

exports.setAttributeNS = function (ns) {
    return function (key) {
        return function (value) {
            return function (element) {
                return function () {
                    element.setAttributeNS(ns, key, value);
                    return {};
                };
            };
        };
    };
};

exports.getAttributeNS = function (ns) {
    return function (key) {
        return function (element) {
            return function () {
                return element.getAttributeNS(ns, key);
            };
        };
    };
};

exports.removeAttributeNS = function (ns) {
    return function (key) {
        return function (element) {
            return function () {
                element.removeAttributeNS(ns, key);
                return {};
            };
        };
    };
};

exports.defaultView = function (htmlDoc) {
    return htmlDoc.defaultView;
};

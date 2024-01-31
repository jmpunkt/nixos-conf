window.domToSvg = require("dom-to-svg");
window.htmlToImage = require("html-to-image");

window.downloadElement = function (elm) {
  const link = document.createElement("a");
  link.download = "save.svg";

  const contents = domToSvg.elementToSVG(elm).rootElement.outerHTML;
  const mime_type = "image/svg+xml";
  const blob = new Blob([contents], { type: mime_type });

  link.href = window.URL.createObjectURL(blob);

  link.click();
  link.remove();
};

window.downloadElement2 = async function (elm) {
  const link = document.createElement("a");
  link.download = "save.svg";
  link.href = await htmlToImage.toSvg(elm);

  link.click();
  link.remove();
};

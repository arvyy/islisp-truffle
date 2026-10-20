'use strict'

const TAG_ALL_RX = /<[^>]+>/g
const QUOT_RX = /"/g

module.exports = (html, { hash: { attribute } }) =>
  html && (attribute ? html.replace(TAG_ALL_RX, '').replace(QUOT_RX, '&quot;') : html.replace(TAG_ALL_RX, ''))

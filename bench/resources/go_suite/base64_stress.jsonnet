{
  local largeStr = std.repeat("Lorem ipsum dolor sit amet, consectetur adipiscing elit. ", 100),
  local encoded = std.base64(largeStr),
  local decoded = std.base64Decode(encoded),
  local encodedArr = std.base64(std.makeArray(1000, function(i) i % 256)),
  local decodedBytes = std.base64DecodeBytes(encodedArr),

  local encoded2 = std.base64(decoded),
  local decoded2 = std.base64Decode(encoded2),
  local encodedArr2 = std.base64(std.makeArray(2000, function(i) (i * 7 + 13) % 256)),
  local decodedBytes2 = std.base64DecodeBytes(encodedArr2),

  local encoded3 = std.base64(decoded2),
  local decoded3 = std.base64Decode(encoded3),
  local encodedArr3 = std.base64(std.makeArray(3000, function(i) (i * 13 + 37) % 256)),
  local decodedBytes3 = std.base64DecodeBytes(encodedArr3),

  roundtrip_ok: decoded3 == largeStr,
  byte_roundtrip_ok: std.length(decodedBytes3) == 3000,
  encoded_len: std.length(encoded3),
  decoded_len: std.length(decoded3)
}

for (file in list.files('0-Config/1-Setup', full.names=TRUE)) {
  source(file)
}

for (file in list.files('0-Config/2-Functions', full.names=TRUE)) {
  source(file)
}
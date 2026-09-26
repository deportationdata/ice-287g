# Shared helpers; sourcing this file loads every topic file
local({
  for (topic in c("normalize", "dates", "io", "manifests", "acquire", "review", "claims", "geometry")) {
    source(file.path("code", paste0("functions-", topic, ".R")))
  }
})

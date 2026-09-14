# Shared helpers, split by topic; source this file and every helper is defined
local({
  for (topic in c("normalize", "dates", "io", "manifests", "acquire", "review", "claims", "geometry")) {
    source(file.path("code", paste0("functions-", topic, ".R")))
  }
})

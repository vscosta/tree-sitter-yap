#include <napi.h>

typedef struct TSLanguage TSLanguage;

<<<<<<< HEAD
extern "C" TSLanguage *tree_sitter_yap();
=======
extern "C" TSLanguage * tree_sitter_yap();
>>>>>>> 7298c0e9b8af8b62b420c548f1ef9557112636ce

// "tree-sitter", "language" hashed with BLAKE2
const napi_type_tag LANGUAGE_TYPE_TAG = {
  0x8AF2E5212AD58ABF, 0xD5006CAD83ABBA16
};

Napi::Object Init(Napi::Env env, Napi::Object exports) {
    exports["name"] = Napi::String::New(env, "yap");
    auto language = Napi::External<TSLanguage>::New(env, tree_sitter_yap());
    language.TypeTag(&LANGUAGE_TYPE_TAG);
    exports["language"] = language;
    return exports;
}

<<<<<<< HEAD
NODE_API_MODULE(tree_sitter_yap_binding, Init)
=======
NODE_MODULE(tree_sitter_yap_binding, Init)

}  // namespace
>>>>>>> 7298c0e9b8af8b62b420c548f1ef9557112636ce

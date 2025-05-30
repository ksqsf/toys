#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <algorithm>
#include <cmath>
#include <chrono>
#include <cassert>
#include <thread>

extern "C" {
#include "llama.h"
}

using namespace std;
using namespace std::chrono;

class LlamaIME {
private:
    llama_model *model;
    llama_context *ctx;
    unordered_map<string, vector<float>> cache;
    int emb_dim;

public:
    LlamaIME(const string& model_path) {
        llama_backend_init();
        llama_model_params model_params = llama_model_default_params();
        model = llama_model_load_from_file(model_path.c_str(), model_params);
        if (!model) {
            throw std::runtime_error("Failed to load model: " + model_path);
        }
        
        llama_context_params ctx_params = llama_context_default_params();
        ctx_params.n_ctx = 512;
        ctx_params.n_batch = 1024;
        ctx_params.embeddings = true;
        ctx_params.n_threads = std::thread::hardware_concurrency();
        ctx_params.pooling_type = LLAMA_POOLING_TYPE_CLS;
        ctx = llama_init_from_model(model, ctx_params);
        if (!ctx) {
            llama_model_free(model);
            throw std::runtime_error("Failed to create context");
        }

        emb_dim = llama_model_n_embd(model);
        cout << "Model loaded successfully" << endl;
        cout << "Embedding dimension: " << emb_dim << endl;
        cout << "Model has encoder: " << llama_model_has_encoder(model) << endl;
        cout << "Model has decoder: " << llama_model_has_decoder(model) << endl;
    }
    
    ~LlamaIME() {
        if (ctx) llama_free(ctx);
        if (model) llama_model_free(model);
        llama_backend_free();
    }
    
    vector<float> getEmbedding(const string& text) {
        // tokenization
        std::vector<llama_token> tokens(512);
        const llama_vocab* vocab = llama_model_get_vocab(model);
        int n_tokens = llama_tokenize(
            vocab, 
            text.c_str(), 
            text.length(), 
            tokens.data(), tokens.size(), 
            true, // add_bos
            false); // special tokens
        if (n_tokens <= 0) return {};
        tokens.resize(n_tokens);
        
        // batch
        llama_batch batch = llama_batch_init(n_tokens, 0, 1);
        for (int i = 0; i < n_tokens; i++) {
            batch.token[i] = tokens[i];
            batch.pos[i] = i;
            batch.n_seq_id[i] = 1;
            batch.seq_id[i][0] = 0;
            batch.logits[i] = false;
        }
        batch.n_tokens = n_tokens;
        batch.logits[batch.n_tokens - 1] = true;
        
        // encode
        if (llama_encode(ctx, batch) != 0) {
            llama_batch_free(batch);
            return {};
        }

        const float* embd = llama_get_embeddings_seq(ctx,0);
        assert(embd);
        std::vector<float> result(embd, embd + emb_dim);
        llama_batch_free(batch);
        return result;
    }
    
    float similarity(const vector<float>& a, const vector<float>& b) {
        assert(a.size() == b.size());
        
        const float* __restrict__ pa = a.data();
        const float* __restrict__ pb = b.data();
        const size_t n = a.size();
        
        float dot = 0.0f;
        float norm_a = 0.0f;
        float norm_b = 0.0f;
        
        for (size_t i = 0; i < n; ++i){
            dot += pa[i] * pb[i];
            norm_a += pa[i] * pa[i];
            norm_b += pb[i] * pb[i];
        }
        if (norm_a == 0.0f || norm_b == 0.0f) return 0.0f;
        return dot / (sqrtf(norm_a) * sqrtf(norm_b));
    }
};

void test1(LlamaIME &ime)
{
    auto start = high_resolution_clock::now();
    
    string prefix = "數學是一個重要的";
    vector<string> cands = {
        "渴慕",
        "科目",
    };

    vector<float> sentence_emb = ime.getEmbedding(prefix);
    cout << "-----\n";
    cout << "sentence " << prefix << "\n";
    for (const auto& w : cands) {
        vector<float> w_emb = ime.getEmbedding(w);
        float similarity = ime.similarity(sentence_emb, w_emb);
        cout << w << " similarity=" << similarity << "\n";
    }
    auto end = high_resolution_clock::now();
    auto duration = duration_cast<milliseconds>(end - start);
    cout << "Test completed in " << duration.count() << " ms\n";
}

void test2(LlamaIME &ime)
{
    auto start = high_resolution_clock::now();
    
    string prefix = "張小明一直";
    vector<string> cands = {
        "渴慕",
        "科目",
    };

    vector<float> sentence_emb = ime.getEmbedding(prefix);
    cout << "-----\n";
    cout << "sentence " << prefix << "\n";
    for (const auto& w : cands) {
        vector<float> w_emb = ime.getEmbedding(w);
        float similarity = ime.similarity(sentence_emb, w_emb);
        cout << w << " similarity=" << similarity << "\n";
    }
    
    auto end = high_resolution_clock::now();
    auto duration = duration_cast<milliseconds>(end - start);
    cout << "Test completed in " << duration.count() << " ms\n";
}

int main(int argc, char *argv[])
{
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <model.gguf>" << std::endl;
        return 1;
    }
    
    
    LlamaIME ime(argv[1]);
    test1(ime);
    test2(ime);
}

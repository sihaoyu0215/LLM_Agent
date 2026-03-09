from langchain_huggingface import HuggingFaceEmbeddings


def get_embedder(model_name):
    return HuggingFaceEmbeddings(model_name=model_name)
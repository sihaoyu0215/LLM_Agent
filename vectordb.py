from langchain_community.vectorstores import FAISS
from langchain.schema import Document


def build_vector_db(chunks, embedder, db_path):
    """
    chunks: List[dict], each dict has 'lines' (list of str) and 'desc' (str)
    """
    docs = [
        Document(
            page_content=''.join(chunk['lines']),
            metadata={'desc': chunk['desc']}
        )
        for chunk in chunks
    ]
    db = FAISS.from_documents(docs, embedder)
    db.save_local(db_path)


def load_vector_db(embedder, db_path):
    return FAISS.load_local(db_path, embedder, allow_dangerous_deserialization=True)

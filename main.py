import os
from config import EMBEDDING_MODEL, VECTOR_DB_PATH
from chunking import *
from embedder import get_embedder
from vectordb import build_vector_db, load_vector_db
from agent import code_generation_agent, code_review_agent
from IPython.display import display, Markdown, Code
import ipywidgets as widgets


def build_db_from_code_files(code_dir):
    all_chunks = []
    for fname in os.listdir(code_dir):
        if fname.endswith(".for"):
            lines = read_code_file(os.path.join(code_dir, fname))
            chunks = extract_code_chunks(lines, min_chunk_lines=6)
            all_chunks.extend(chunks)
    embedder = get_embedder(EMBEDDING_MODEL)
    build_vector_db(all_chunks, embedder, VECTOR_DB_PATH)


def rag_context(query, vector_db, top_k=3):
    retrieved = vector_db.similarity_search(query, k=top_k)
    context = "\n\n".join([doc.page_content for doc in retrieved])
    return context


def hybrid_rag_multiagent_workflow(question, vector_db, max_rounds=1):
    feedback = None
    for round_idx in range(1, max_rounds + 1):
        display(Markdown(f"---\n### Round {round_idx}\n"))
        rag_ctx = rag_context(question, vector_db)
        display(Markdown("**RAG检索上下文:**"))
        display(Code(rag_ctx, language="fortran"))
        code, result = code_generation_agent(question, rag_ctx, feedback)
        is_correct, feedback = code_review_agent(question, code)
        display(Markdown("**Generated code:**"))
        if code:
            display(Code(code, language="python"))
            display(result)

        if is_correct:
            display(Markdown("<span style='color:green'>Review: correct! Code accepted.</span>"))
            return code
        else:
            display(Markdown(f"<span style='color:red'>Review: incorrect.</span><br>**Feedback:** {feedback}"))
    display(Markdown("<span style='color:red'>Max rounds reached. No correct code generated.</span>"))
    return None


def show_prompt_instruction():
    display(Markdown("""
**Please structure your prompt as follows:**

Given the following yield surface equation: `{replace with your actual yield surface equation}`  
and the following plastic potential surface equation: `{replace with your plastic potential surface equation}`,  
where `{necessary explanations for the symbols used in the above equations}`.

Please write a complete ABAQUS UMAT user material subroutine file in Fortran that implements a plasticity model based on these surfaces. The UMAT should:
- Clearly define and read all necessary material parameters from the PROPS array.
- Evaluate the yield function and determine whether the material response is elastic or plastic.
- For plastic loading, apply a suitable return mapping algorithm (semi-implicit scheme) to enforce the yield condition and update the stress state accordingly.
- Update all relevant state variables (e.g., equivalent plastic strain).
- Return the updated stress, elastic stiffness matrix (DDSDDE), and state variables.
- Include clear comments explaining each major step and the physical meaning of key variables.

*Please fill in the equations and explanations in the brackets above before submitting your prompt.*
"""))


def interactive_qa():
    embedder = get_embedder(EMBEDDING_MODEL)
    vector_db = load_vector_db(embedder, VECTOR_DB_PATH)
    display(Markdown("## Hybrid RAG + Multi-agent Code Generation System"))
    show_prompt_instruction()
    question_box = widgets.Textarea(
        value='',
        placeholder='Describe your code task...',
        description='Task:',
        disabled=False,
        layout=widgets.Layout(width='70%'),
        rows=2
    )
    run_button = widgets.Button(description="Run", button_style="success")
    output = widgets.Output()

    """
    Adaptively adjust the height
    """
    from IPython.display import Javascript
    display(Javascript("""
    document.querySelectorAll('textarea').forEach(function(textarea){
        textarea.style.height = 'auto';
        textarea.addEventListener('input', function(){
            this.style.height = 'auto';
            this.style.height = (this.scrollHeight) + 'px';
        });
    });
    """))

    def on_run_clicked(b):
        with output:
            output.clear_output()
            question = question_box.value.strip()
            if not question:
                display(Markdown("**Please enter a code task!**"))
                return
            code = hybrid_rag_multiagent_workflow(question, vector_db)
            if code:
                display(Markdown("### Final reviewed code"))
                display(Code(code, language="fortran"))
            else:
                display(
                    Markdown("<span style='color:orange'>Failed to generate correct code after several rounds.</span>"))

    run_button.on_click(on_run_clicked)
    display(widgets.HBox([question_box, run_button]))
    display(output)


if __name__ == "__main__":
    # build_db_from_code_files("./data")  # Only run this code to initialize your db
    interactive_qa()

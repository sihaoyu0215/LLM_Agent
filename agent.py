from llm import call_llm

GENERATOR_SYSTEM_PROMPT = (
    "You are an expert in ABAQUS UMAT user subroutine development. Use the following context as reference and my task description to write a complete and usable Fortran UMAT subroutine. "
    "You will be provided with relevant code context and (optionally) reviewer feedback. "
    "Write clear, correct, well-documented code that solves the user's task."
)
REVIEWER_SYSTEM_PROMPT = (
    "You are a senior code reviewer. "
    "Review the provided Fortran UMAT subroutine code for correctness and quality. Your task is to check if the code has any formatting errors, logical mistakes, or functional issues. "
    "If the code is fully correct, reply only with 'correct'. "
    "If not, reply 'incorrect: <reason and suggestions>'."
)


def code_generation_agent(question, rag_context, feedback=None):
    """
    Generates code given the question, RAG context, and optional reviewer feedback.
    """
    user_prompt = f"Relevant code context:\n{rag_context}\n\n"
    if feedback:
        user_prompt += f"Previous review feedback: {feedback}\n\n"
    user_prompt += (
        f"Task: {question}\n"
        "Please write the code.\n"
        "You may use information from the context and feedback above as references, "
        "but you are not limited to them. Feel free to use your own knowledge to provide a complete solution."
    )
    code, reason = call_llm(user_prompt, system_prompt=GENERATOR_SYSTEM_PROMPT)
    return code, reason


def code_review_agent(question, code):
    """
    Reviews code. Returns (is_correct, feedback)
    """
    user_prompt = (
        f"Task: {question}\n"
        f"Code:\n{code}\n"
        f"Carefully review the provided code based on the task above\n"
        "If the code fully and correctly solves the task, reply with 'correct'.\n"
        "If there are any issues, reply exactly in this format:\n"
        "incorrect: <briefly explain what is wrong and provide suggestions for improvement.>\n"
        "Do not include any explanation outside the specified format."
    )
    response, result = call_llm(user_prompt, system_prompt=REVIEWER_SYSTEM_PROMPT)
    response_lower = response.strip().lower()
    if response_lower.startswith("correct"):
        return True, ""
    else:
        # extract the explanation after the colon in the model’s response
        reason = response.partition(':')[2].strip() if ':' in response else response
        return False, reason

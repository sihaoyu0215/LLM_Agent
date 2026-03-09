# openai_api.py
from openai import OpenAI
from config import OPENAI_API_KEY
import base64
import filetype

client = OpenAI(api_key=OPENAI_API_KEY)


def call_llm(prompt, system_prompt=None, model="gpt-4.1", temperature=0.5, top_p=0.7, max_tokens=2048,
             image_path=None):
    """
    Args:
        prompt: Text prompt
        system_prompt: System prompt (role=system)
        model: Model name
        temperature
        top_p
        max_tokens
        image_path: Path of base64-encoded image (optional)
    """
    messages = []
    if system_prompt:
        messages.append({"role": "system", "content": system_prompt})

    if image_path:
        kind = filetype.guess(image_path)
        if kind is None or kind.mime.split('/')[0] != 'image':
            raise ValueError(f"Unsupported image file type: {kind}")
        image_type = kind.extension
        with open(image_path, "rb") as f:
            image_base64 = base64.b64encode(f.read()).decode()
        image_data_url = f"data:image/{image_type};base64,{image_base64}"
        messages.append({
            "role": "user",
            "content": [
                {"type": "text", "text": prompt},
                {"type": "image_url", "image_url": {"url": image_data_url}}
            ]
        })
    else:
        messages.append({
            "role": "user",
            "content": prompt
        })
    response = client.chat.completions.create(
        model=model,
        messages=messages,
        top_p=top_p,
        temperature=temperature,
        stream=False,
        max_tokens=max_tokens,
    )
    return response.choices[0].message.content, response.choices[0].finish_reason


if __name__ == '__main__':
    msg, reason = call_llm(prompt="please introduce your model version")
    print(msg, reason)

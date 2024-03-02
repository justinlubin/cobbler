import huggingface_hub

def login():
    # TODO switch to environment variable
    TOKEN = "TODO"
    huggingface_hub.login(token=TOKEN)

if __name__ == "__main__":
    login()

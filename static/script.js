// variaveis com os elementos principais do html
const sendButton   = document.getElementById("send-button");
const fromInput    = document.getElementById("from");
const toInput      = document.getElementById("to");
const contentInput = document.getElementById("content");
const contactList  = document.getElementById("contacts-list");
const chatArea     = document.getElementById("chat-area");

let currentUser = ""

// fica "escutando" o input de usuario e atualizando os
// contatos com base no usuario
// provisório -> posteriormente não terá como mudar de usuário
fromInput.addEventListener("input", (e) => {
    currentUser = e.target.value.trim();
    // loadContacts(); // TODO: 
});

// fica "escutando" o botao, quando clicado vai enviar a mensagem
// para o endpoint "/msg"
sendButton.addEventListener("click", async () => {
    const from    = fromInput.value.trim();
    const to      = toInput.value.trim();
    const content = contentInput.value.trim();

    if (!from || !to || !content) {
        alert("Preencha todos os campos antes de enviar.");
        return;
    }

    const message = {
        user_from:   from,
        user_to:     to,
        msg_content: content
    };

    try {
        const res = await fetch("/msg", {
            method:  "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(message)
        });

        if (res.ok) {
            const data = await res.json();
            contentInput.value = "";
            // loadContacts();
            // loadConversation(from, to);
        } else {
            alert("erro ao enviar a mensagem");
        }
    } catch (err) {
        console.error("erro: ", err);
        alert("erro de rede ao enviar a mensagem");
    }
});

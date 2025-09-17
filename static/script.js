// variaveis com os elementos principais do html
const sendButton   = document.getElementById("send-button");
const fromInput    = document.getElementById("from");
const toInput      = document.getElementById("to");
const contentInput = document.getElementById("content");
const contactsList  = document.getElementById("contacts-list");
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

// carrega a lista de contatos de um respectivo usuario
// e adiciona na interface (formata)
async function loadContacts() {
    if (!currentUser) return;
    
    try {
        const res          = await fetch("/msgs");
        const all_messages = await res.json();
    
        const contactsSet  = new Set();

        all_messages.forEach(msg => {
            if (msg.user_from === currentUser) contactsSet.add(msg.user_to);
            if (msg.user_to   === currentUser) contactsSet.add(msg.user_to)
        });

        contactsList.innerHTML = "";

        contactsSet.forEach(contact => {
            const li = document.createElement("li");
            li.textContent = contact;
            li.style.cursor = "pointer";
            li.addEventListener("click", () => {
                toInput.value = contact;
                loadConversation(currentUser, contact);
            });
            contactsList.appendChild(li);
        });
    } catch (err) {
        console.log("erro ao carregar contatos", err);
    }
}

// carrega a conversa entre dois usuarios
// e adiciona na interface
async function loadConversation(user1, user2) {
    try {
        const res = await fetch (`/chat/${user1}/${user2}`);
        if (res.ok) {
            chatArea.innerHTML = "<p>erro ao carregar conversa</p>"
            return;
        }

        const messages = await res.json();

        chatArea.innerHTML = "";

        messages.forEach(msg => {
            const p = document.createElement("p");
            p.textContent = `${msg.user_from}: ${msg.msg_content}`
            chatArea.appendChild(p);
        });
    } catch (err) {
        console.error("erro ao carregar conversa", err);
    }
}
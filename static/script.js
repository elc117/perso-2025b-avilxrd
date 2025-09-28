// variaveis com os elementos principais do html
const sendButton              = document.getElementById("send-button");
const fromInput               = document.getElementById("from");

const contentInput            = document.getElementById("content");
const contactsList            = document.getElementById("contacts-list");
const chatArea                = document.getElementById("chat-area");
const msgsArea                = document.getElementById("messages-area");
const contactSearch           = document.getElementById("contact-search")
const newConversationButton   = document.getElementById("new-conversation-button");
const newConversationModal    = document.getElementById("new-conversation");
const newContactInput         = document.getElementById("new-contact");
const startConversationButton = document.getElementById("start-conversation-button");

let toInput;
let currentUser = "Miguel"
let contact
loadContacts();

// evento de click para adicionar uma nova conversa
newConversationButton.addEventListener("click", () => {
    if (newConversationModal.style.display === "block") newConversationModal.style.display = "none";
    else newConversationModal.style.display = "block";
});

// inicia a conversa com o contato especificado
startConversationButton.addEventListener("click", () => {
    const newContact = newContactInput.value.trim();
    if (newContact) {
        toInput = newContact;
        loadConversation(currentUser, newContact);
        document.getElementById("contact-name").innerHTML = newContact;
        newContactInput.value = "";
        newConversationModal.style.display = "none";
    } else alert("Por favor, digite um nome de contato.");
});

// evento para a filtragem dos contatos
contactSearch.addEventListener("input", () => {
    const searchTerm = contactSearch.value.trim().toLowerCase();
    filterContacts(searchTerm);
});

// função para filtrar contatos
function filterContacts(searchTerm) {
    const contactDivs = contactsList.getElementsByClassName("contact");
    
    Array.from(contactDivs).forEach(contactDiv => {
        const contactName = contactDiv.querySelector("a").textContent.toLowerCase();
        if (contactName.includes(searchTerm)) {
            contactDiv.style.display = "";
        } else contactDiv.style.display = "none";
    });
}

// // fica "escutando" o input de usuario e atualizando os
// // contatos com base no usuario
// // provisório -> posteriormente não terá como mudar de usuário
// fromInput.addEventListener("input", (e) => {
//     currentUser = e.target.value.trim();
//     loadContacts();
// });

// fica "escutando" o botao, quando clicado vai enviar a mensagem
// para o endpoint "/msg"
sendButton.addEventListener("click", async () => {
    // const from    = fromInput.value.trim();
    const from    = currentUser
    const to      = toInput.trim();
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
            loadContacts();
            loadConversation(from, to);
        } else alert("erro ao enviar a mensagem");

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
        const res = await fetch("/msgs");
        const all_messages = await res.json();
    
        const contactsSet = new Set();

        all_messages.forEach(msg => {
            if (msg.user_from === currentUser && msg.user_to !== currentUser) contactsSet.add(msg.user_to);
            if (msg.user_to === currentUser && msg.user_from !== currentUser) contactsSet.add(msg.user_from);

        });

        contactsList.innerHTML = "";

        async function getLastMessage(user1, user2) {
            try {
                const response = await fetch(`/chat/${user1}/${user2}/last`);
                if (!response.ok) {
                    throw new Error('Network response was not ok');
                }
                const lastMessage = await response.json();
                return lastMessage ? lastMessage.msg_content : "No messages yet";
            } catch (error) {
                console.error('Error fetching last message:', error);
                return "Error loading message";
            }
        }

        // Carregar contatos e suas últimas mensagens
        for (const contact of contactsSet) {
            const contactDiv = document.createElement("div");
            contactDiv.className = "contact";

            const picFrame = document.createElement("div");
            picFrame.className = "pic-frame user-fem";

            const nameLink = document.createElement("a");
            nameLink.textContent = contact;

            let lastMessageContent = await getLastMessage(currentUser, contact);
            const messageLink = document.createElement("a");

            if (lastMessageContent.length + contact.length > 30) lastMessageContent = lastMessageContent.substring(0, 25) + '...';

            messageLink.textContent = ": " + lastMessageContent;

            contactDiv.appendChild(picFrame);
            contactDiv.appendChild(nameLink);
            contactDiv.appendChild(messageLink);
            
            contactDiv.style.cursor = "pointer";
            contactDiv.addEventListener("click", () => {
                // toInput.value = contact;
                toInput = contact;
                loadConversation(currentUser, contact);
                let cname = document.getElementById("contact-name");
                cname.innerHTML = contact;    
            });
            contactsList.insertBefore(contactDiv, contactsList.firstChild);
        }
    } catch (err) {
        console.log("Erro ao carregar contatos", err);
    }
}

// carrega a conversa entre dois usuarios
// e adiciona na interface
async function loadConversation(user1, user2) {
    try {
        const res = await fetch (`/chat/${user1}/${user2}`);
        if (!res.ok) {
            msgsArea.innerHTML = "<p>erro ao carregar conversa</p>"
            return;
        }

        const messages = await res.json();

        msgsArea.innerHTML = "";

        messages.forEach(msg => {
            const div = document.createElement("div");
            if (msg.user_from == currentUser) div.className = "message sent"
            else div.className = "message received"

            const p = document.createElement("p");
            p.textContent = `${msg.msg_content}`
            div.append(p);
            msgsArea.appendChild(div);
        });
    } catch (err) {
        console.error("erro ao carregar conversa", err);
    }
}
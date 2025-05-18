CREATE TABLE users (
    id UUID NOT NULL PRIMARY KEY,
    email VARCHAR(128) NOT NULL UNIQUE,
    password_hash VARCHAR(256) NOT NULL
);
CREATE INDEX users_email_index ON users(email);

CREATE TABLE boards (
    id UUID NOT NULL PRIMARY KEY,
    board_name VARCHAR(64) NOT NULL,
    user_id UUID NOT NULL,
    CONSTRAINT fk_boards_users FOREIGN KEY (user_id) REFERENCES users(id),
    CONSTRAINT unique_board_users_id UNIQUE (board_name, user_id)
);

CREATE TABLE task_status (
    id INT NOT NULL PRIMARY KEY,
    status_name VARCHAR(32) NOT NULL
);
INSERT INTO task_status(id, status_name) VALUES 
    (1, 'Open'),
    (2, 'In progress'),
    (3, 'Merge request'),
    (4, 'QA'),
    (5, 'Done'),
    (6, 'Cancelled');

CREATE TABLE tasks (
    id UUID NOT NULL PRIMARY KEY,
    board_id UUID NOT NULL,
    created_by UUID NOT NULL,
    status_id INT NOT NULL DEFAULT 1,
    assigned UUID,
    title VARCHAR(64) NOT NULL,
    contents VARCHAR(512) NOT NULL,
    created_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now(),
    updated_at TIMESTAMP WITHOUT TIME ZONE NOT NULL DEFAULT now(),
    CONSTRAINT fk_tasks_boards FOREIGN KEY (board_id) REFERENCES boards(id),
    CONSTRAINT fk_tasks_created_by FOREIGN KEY (created_by) REFERENCES users(id),
    CONSTRAINT fk_tasks_status FOREIGN KEY (status_id) REFERENCES task_status(id),
    CONSTRAINT fk_tasks_assigned FOREIGN KEY (assigned) REFERENCES users(id)
);

CREATE OR REPLACE FUNCTION update_tasks_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW;
END;
$$ language plpgsql;

CREATE TRIGGER trigger_tasks_update
BEFORE UPDATE ON tasks
FOR EACH ROW
EXECUTE FUNCTION update_tasks_updated_at_column();

CREATE OR REPLACE FUNCTION register_user(_id UUID, _email VARCHAR, _password_hash VARCHAR)
RETURNS BOOLEAN as $$
BEGIN
    IF EXISTS (SELECT email FROM users WHERE email = _email)
        THEN 
            RETURN FALSE;
        ELSE
            INSERT INTO users(id, email, password_hash)
            VALUES(_id, _email, _password_hash);
            RETURN TRUE;
    END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION add_board(_id UUID, _board_name VARCHAR, _user_id UUID)
RETURNS BOOLEAN as $$
BEGIN
    IF EXISTS (SELECT id FROM boards WHERE board_name = _board_name AND user_id = _user_id)
        THEN 
            RETURN FALSE;
        ELSE
            INSERT INTO boards(id, board_name, user_id)
            VALUES(_id, _board_name, _user_id);
            RETURN TRUE;
    END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION create_task(_id UUID, _board_id UUID, _created_by UUID, _title VARCHAR, _contents VARCHAR)
RETURNS VARCHAR as $$
DECLARE
    owner_id UUID;
BEGIN
    SELECT user_id INTO owner_id FROM boards WHERE id = _board_id;
    IF owner_id IS NULL
        THEN RETURN 'not found'; 
    END IF;
    IF owner_id = _created_by
        THEN 
            INSERT INTO tasks(id, board_id, created_by, title, contents)
            VALUES(_id, _board_id, _created_by, _title, _contents);
            RETURN 'created';
    ELSE
        RETURN 'forbidden';
    END IF;
END;
$$ LANGUAGE plpgsql;

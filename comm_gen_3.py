import random
from datetime import datetime, timedelta
import csv

# Поведенческие профили
authors = [
    {"name": "Alice", "email": "alice@example.com", "languages": ["Python"], "active_hours": (9, 17)},
    {"name": "Bob", "email": "bob@example.com", "languages": ["JavaScript"], "active_hours": (12, 20)},
    {"name": "Carol", "email": "carol@example.com", "languages": ["Python", "JavaScript"], "active_hours": (10, 18)},
]

# Файлы проекта
file_dict = {
    f"src/file_{i}.{'py' if i % 2 == 0 else 'js'}": {
        "lines": 0,
        "exists": False,
        "language": "Python" if i % 2 == 0 else "JavaScript"
    } for i in range(1, 21)
}

# Сообщения
messages = ["Refactor code", "Fix bug", "Improve docs", "Add feature", "Remove unused imports", "Initial commit"]

# Стартовая дата
base_date = datetime(2024, 2, 1)
commits = []

def generate_next_time(current_time):
    return current_time + timedelta(hours=random.randint(6, 36))

# Генерация коммитов для одного файла
def generate_commits_for_file(file_name, file_meta):
    current_time = base_date + timedelta(days=random.randint(0, 5))
    file_commits = []

    for _ in range(random.randint(5, 15)):
        language = file_meta["language"]
        valid_authors = [a for a in authors if language in a["languages"]]
        author = random.choice(valid_authors)

        # Переход ко времени
        commit_time = generate_next_time(current_time)
        current_time = commit_time

        commit = {
            "commit_id": f"{random.getrandbits(32):08x}",
            "authored_date": commit_time.strftime("%Y-%m-%d %H:%M:%S"),
            "author_name": author["name"],
            "author_email": author["email"],
            "message": random.choice(messages),
            "new_path": file_name,
            "old_path": file_name,
            "lines_added": 0,
            "lines_removed": 0,
            "new_file": False,
            "renamed_file": False,
            "deleted_file": False,
            "file_language": language
        }

        if not file_meta["exists"]:
            # создаём файл
            commit["new_file"] = True
            commit["lines_added"] = random.randint(30, 200)
            file_meta["lines"] = commit["lines_added"]
            file_meta["exists"] = True

        else:
            roll = random.random()
            if roll < 0.05:
                # удаление
                commit["deleted_file"] = True
                commit["lines_removed"] = file_meta["lines"]
                file_meta["lines"] = 0
                file_meta["exists"] = False

            elif roll < 0.25:
                # переименование (для реализма можно просто менять имя)
                commit["renamed_file"] = True
                commit["lines_added"] = random.randint(5, 50)
                commit["lines_removed"] = random.randint(0, min(file_meta["lines"], 30))
                file_meta["lines"] = max(0, file_meta["lines"] + commit["lines_added"] - commit["lines_removed"])
                # изменим путь
                new_name = file_name.replace(".", f"_{random.randint(1,99)}.")
                commit["new_path"] = new_name
                file_dict[new_name] = file_meta
                del file_dict[file_name]
                file_name = new_name

            else:
                # обычный коммит
                commit["lines_added"] = random.randint(1, 50)
                commit["lines_removed"] = random.randint(0, min(file_meta["lines"], 30))
                file_meta["lines"] = max(0, file_meta["lines"] + commit["lines_added"] - commit["lines_removed"])

        file_commits.append(commit)

        # если файл удалён — выходим
        if not file_meta["exists"]:
            break

    return file_commits

# Запускаем генерацию по всем файлам
for file_name, file_meta in list(file_dict.items()):
    commits.extend(generate_commits_for_file(file_name, file_meta))

# Сохраняем в CSV
with open("synthetic_commits.csv", "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=[
        "commit_id", "authored_date", "author_name", "author_email",
        "message", "new_path", "old_path", "lines_added", "lines_removed",
        "new_file", "renamed_file", "deleted_file", "file_language"
    ])
    writer.writeheader()
    writer.writerows(commits)

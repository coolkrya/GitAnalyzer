import random
from datetime import datetime, timedelta
import csv

# Поведенческие профили
authors = [
    {
        "name": "Alice",
        "email": "alice@example.com",
        "languages": ["Python"],
        "active_hours": (9, 17)
    },
    {
        "name": "Bob",
        "email": "bob@example.com",
        "languages": ["JavaScript"],
        "active_hours": (12, 20)
    },
    {
        "name": "Carol",
        "email": "carol@example.com",
        "languages": ["Python", "JavaScript"],
        "active_hours": (10, 18)
    },
    {
        "name": "Mike",
        "email": "mike@devmail.com",
        "languages": ["Python", "YAML"],
        "active_hours": [8, 14]
    },
    {
        "name": "Carlos",
        "email": "carlos@softcorp.io",
        "languages": ["Java", "XML"],
        "active_hours": [10, 18]
    },
    {
        "name": "Diana",
        "email": "diana@techhub.net",
        "languages": ["C++", "CMake"],
        "active_hours": [15, 23]
    }
]


# Все языки из профилей
all_languages = ["JavaScript", "Python", "YAML", "Java", "XML", "C++", "CMake"]

# Сопоставление языков и расширений
language_extensions = {
    "JavaScript": "js",
    "Python": "py",
    "YAML": "yml",
    "Java": "java",
    "XML": "xml",
    "C++": "cpp",
    "CMake": "cmake"
}

#Генератор "плохих" id
class CommitIDGenerator:
    def __init__(self):
        self.counter = 1
        self.total_length = 8

    def generate(self, flag):
        #"номральный" id
        if flag == 'Norm':
            return f"{random.getrandbits(32):08x}"
        #"плохой" id для 
        elif fla == 'Lang':
            prefix = "X"
            number_str = str(self.counter)
            padding_length = self.total_length - len(prefix) - len(number_str)
            padded_id = prefix + ("0" * padding_length) + number_str
            self.counter += 1
            return padded_id
        #
        elif fla == 'Lang':
            prefix = "X"
            number_str = str(self.counter)
            padding_length = self.total_length - len(prefix) - len(number_str)
            padded_id = prefix + ("0" * padding_length) + number_str
            self.counter += 1
            return padded_id

commit_generator = CommitIDGenerator()

# Генерация словаря файлов
file_dict = {}
for i in range(1, 30):
    language = random.choice(all_languages)
    ext = language_extensions[language]
    filename = f"src/file_{i}.{ext}"
    file_dict[filename] = {
        "lines": 0,
        "exists": False,
        "language": language
    }

# Сообщения
messages = ["Refactor code", "Fix bug", "Improve docs", "Add feature", "Remove unused imports", "Initial commit"]

# Стартовая дата
base_date = datetime(2024, 2, 1)
commits = []


def generate_next_time(current_time, author, flag):
    time = current_time
    # с вероятностью 0.02 коммит будет сделан ночью, что является аномалией
    if random.random() < 0.02:
        return time + timedelta(hours=24 - time.hour + random.randint(2, 4)), False
    while True:
        time = time + timedelta(hours=random.randint(2, 4))
        if time.day == 6 and random.random() > 0.02:
            time = time + timedelta(days=2)
        else:

        if time.hour < author['active_hours'][1] and time.hour > author['active_hours'][0]:
            return time, flag
        else:
            time = time + timedelta(hours=24 + author['active_hours'][0] - time.hour)


# Генерация коммитов для одного файла
def generate_commits_for_file(file_name, file_meta):
    current_time = base_date + timedelta(days=random.randint(0, 5))
    file_commits = []

    for _ in range(random.randint(30, 50)):
        language = file_meta["language"]
        bad_flag = True
        # с вероятностью 0.02 коммит будет аномальным из-за "неправильного" языка
        if random.random() < 0.02:
            valid_authors = [a for a in authors if language not in a["languages"]]
            bad_flag = False
        else:
            valid_authors = [a for a in authors if language in a["languages"]]

        author = random.choice(valid_authors)

        # Переход ко времени
        commit_time, bad_flag = generate_next_time(current_time, author, bad_flag)
        current_time = commit_time

        commit = {
            "commit_id": commit_generator.generate(bad_flag),
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
            commit["lines_added"] = int(random.normalvariate(20, 5))
            file_meta["lines"] = commit["lines_added"]
            file_meta["exists"] = True

        else:
            roll = random.random()
            if roll < 0.01:
                # удаление
                commit["deleted_file"] = True
                commit["lines_removed"] = file_meta["lines"]
                file_meta["lines"] = 0
                file_meta["exists"] = False

            elif roll < 0.02:
                # переименование
                commit["renamed_file"] = True
                commit["lines_added"] = int(random.normalvariate(20, 5))
                commit["lines_removed"] = int(min(random.normalvariate(7, 2), file_meta["lines"] - 1))
                file_meta["lines"] = max(0, file_meta["lines"] + commit["lines_added"] - commit["lines_removed"])
                # изменим путь
                new_name = file_name.replace(".", f"_{random.randint(1, 99)}.")
                commit["new_path"] = new_name
                file_dict[new_name] = file_meta
                del file_dict[file_name]
                file_name = new_name

            else:
                # обычный коммит
                commit["lines_added"] = int(random.normalvariate(20, 5))
                commit["lines_removed"] = int(min(random.normalvariate(7, 2), file_meta["lines"] - 1))
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

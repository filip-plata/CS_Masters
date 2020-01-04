from datetime import datetime
from enum import IntEnum

#from django.db import models
from django.conf import settings
from django.db import models
from django.db.models.base import ModelBase, Model


class MyModelBase( ModelBase ):
    def __new__( cls, name, bases, attrs, **kwargs ):
        if name != "MyModel":
            class MetaB:
                db_table = name


            attrs["Meta"] = MetaB

        r = super().__new__( cls, name, bases, attrs, **kwargs )
        return r


class MyModel( Model, metaclass = MyModelBase ):
    class Meta:
        abstract = True

class DeckType(MyModel):
    typeName = models.CharField(max_length=50, unique=True)

class BattleType(MyModel):
    typeName = models.CharField(max_length=50, unique=True)

class PlayerTypeId(MyModel):
    typeName = models.CharField(max_length=50, unique=True)

class ModeDeckType(MyModel):
    typeName = models.CharField(max_length=50, unique=True)

class CardLevelsType(MyModel):
    typeName = models.CharField(max_length=50, unique=True)

class BattleMode(MyModel):
    overtimeSeconds = models.IntegerField()
    playerType = models.ForeignKey(PlayerTypeId, on_delete=models.CASCADE)
    modeDeckType = models.ForeignKey(ModeDeckType, on_delete=models.CASCADE)
    sameDeck = models.BooleanField()
    cardLevels = models.ForeignKey(CardLevelsType, on_delete=models.CASCADE)

class Team(MyModel):
    crownsEarned = models.IntegerField()

class Arena(MyModel):
    id = models.IntegerField(primary_key=True)

class Battle(MyModel):
    id = models.CharField(max_length=100, primary_key=True)
    team = models.ForeignKey(Team, on_delete=models.CASCADE, related_name='team')
    opponentTeam = models.ForeignKey(Team, on_delete=models.CASCADE, related_name='opponentTeam')
    time = models.DateTimeField()
    arena = models.ForeignKey(Arena, on_delete=models.CASCADE)
    deck = models.ForeignKey(DeckType, on_delete=models.CASCADE)
    type = models.ForeignKey(BattleType, on_delete=models.CASCADE)
    mode = models.ForeignKey(BattleMode, on_delete=models.CASCADE)

class CardType(MyModel):
    name = models.CharField(max_length=50, unique=True)

class Card(MyModel):
    key = models.CharField(max_length=50)
    cardType = models.ForeignKey(CardType, on_delete=models.CASCADE, null=True)

class Clan(MyModel):
    tag = models.CharField(max_length=100, unique=True)
    name = models.CharField(max_length=50)

class ClanRoleType(MyModel):
    roleName = models.CharField(max_length=50, unique=True)

class ClanMembership(MyModel):
    clan = models.ForeignKey(Clan, on_delete=models.CASCADE)
    role = models.ForeignKey(ClanRoleType, on_delete=models.CASCADE)
    donations = models.IntegerField()
    donationsReceived = models.IntegerField()

class Player(MyModel):
    id = models.CharField(max_length=100, primary_key=True)
    tag = models.CharField(max_length=100, unique=True)

class PlayerGameStats(MyModel):
    total = models.IntegerField()
    wins = models.IntegerField()
    draws = models.IntegerField()
    tournamentGames = models.IntegerField()
    warDayWins = models.IntegerField()

class PlayerStats(MyModel):
    player = models.ForeignKey(Player, on_delete=models.CASCADE)
    name = models.CharField(max_length=50)
    trophies = models.IntegerField()
    arena = models.ForeignKey(Arena, on_delete=models.CASCADE)
    tournamentCardsWon = models.IntegerField()
    maxTrophies = models.IntegerField()
    totalDonations = models.IntegerField()
    level = models.IntegerField()
    favouriteCard = models.ForeignKey(Card, on_delete=models.CASCADE)
    timestamp = models.DateTimeField(blank=True, null=True)
    clanMembership = models.OneToOneField(ClanMembership, on_delete=models.CASCADE)
    gameStats = models.OneToOneField(PlayerGameStats, on_delete=models.CASCADE)

class TeamMember(MyModel):
    team = models.ForeignKey(Team, on_delete=models.CASCADE)
    player = models.ForeignKey(Player, on_delete=models.CASCADE)
    clan = models.ForeignKey(Clan, on_delete=models.CASCADE)
    name = models.CharField(max_length=50)

    class Meta:
        unique_together = ('team', 'player',)


class MemberBattleCard(MyModel):
    member = models.ForeignKey(TeamMember, on_delete=models.CASCADE)
    card = models.ForeignKey(Card, on_delete=models.CASCADE)
    level = models.IntegerField()

class PlayerCard(MyModel):
    playerStats = models.ForeignKey(PlayerStats, on_delete=models.CASCADE)
    card = models.ForeignKey(Card, on_delete=models.CASCADE)
    level = models.IntegerField()
    count = models.IntegerField()

class PlayerBattleDeck(MyModel):
    playerStats = models.ForeignKey(PlayerStats, on_delete=models.CASCADE)
    card = models.ForeignKey(Card, on_delete=models.CASCADE)

class Achievement(MyModel):
    name = models.CharField(max_length=50, unique=True)

class PlayerAchievement(MyModel):
    playerStats = models.ForeignKey(PlayerStats, on_delete=models.CASCADE)
    achievement = models.ForeignKey(Achievement, on_delete=models.CASCADE)
    stars = models.IntegerField()
    value = models.IntegerField()
    target = models.IntegerField()

    class Meta:
        unique_together = ('playerStats', 'achievement',)
FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
WORKDIR /src
COPY *.fsproj .
RUN dotnet restore
COPY . .
RUN dotnet publish -c Release -o /app/publish

FROM mcr.microsoft.com/dotnet/runtime:8.0
WORKDIR /app
COPY --from=build /app/publish .
RUN mkdir -p /app/temp
RUN chmod 777 /app/temp
ENV DEPLOY_MODE=dev
RUN useradd -m appuser && chown -R appuser:appuser /app
USER appuser
ENTRYPOINT ["dotnet", "QuoteBot.dll"]